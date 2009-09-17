{-# LANGUAGE FlexibleInstances #-}
module VM where

import VM.Util

import qualified Data.Map as M
import Control.Arrow
import System.IO
import Control.Monad
import Data.BitString
import Control.Applicative
import Data.Foldable (toList)
import Data.List (inits)
import Data.List.Split (splitPlaces)
import System.IO.Unsafe

type Bits = [Bool]

data Instruction = Instruction {
    iArity :: [Int],
    iFunc :: [Bits] -> State -> State
}

instTable :: M.Map String Bits
instTable = M.fromList $ map fst instDefs

inst :: M.Map Bits Instruction
inst = M.fromList $ map (first snd) instDefs

instDefs :: [ ((String, [Bool]), Instruction) ]
instDefs = [
        (("mov", toB "00"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ regGet state src
        }),
        (("loadK", toB "00"), Instruction {
            iArity = [6,10],
            iFunc = \[dst,size] state ->
                let bits = takeFromIp state (bToI size)
                in (flip rJump $ bToI size) $ regSet state dst bits
        }),
        (("add", toB "0110"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ (regGet state x) + (regGet state y)
        }),
        (("sub", toB "0111"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ (regGet state x) + (regGet state y)
        }),
        (("length", toB "11110"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ iToB $ length $ regGet state src
        }),
        (("exitR", toB "111110"), Instruction {
            iArity = [6],
            iFunc = \[x] state ->
                Terminated $ regGet state x
        }),
        (("exitI", toB "111111"), Instruction {
            iArity = [32],
            iFunc = \[bits] _ -> Terminated bits
        })
    ]

type Register = Bits
-- pair up some words with the 6-digit binary counting set
regTable :: M.Map String Register
regTable = M.fromList
    $ zip ("ip" : "acc" : (map (:[]) $ ['a' .. 'z'] ++ ['α' .. 'ω']))
    $ replicateM 6 [False,True]

type Registers = M.Map Register Bits
initRegisters :: Registers
initRegisters = M.fromList $ zip (M.elems regTable) $ repeat []

regGet :: State -> Register -> Bits
regGet state reg = (head $ registerStack state) M.! reg

regGet' :: State -> String -> Bits
regGet' state name = regGet state $ regTable M.! name

regSet :: State -> Register -> Bits -> State
regSet state reg bits = state {
        registerStack = (M.insert reg bits $ head $ registerStack state)
            : (tail $ registerStack state)
    }

regSet' :: State -> String -> Bits -> State
regSet' state name bits = regSet state (regTable M.! name) bits

regAdjust :: State -> Register -> (Bits -> Bits) -> State
regAdjust t@(Terminated _) _ _ = t
regAdjust state reg adjuster = state {
        registerStack = adjustHead (registerStack state)
            $ M.adjust adjuster reg
    }

adjustHead :: [a] -> (a -> a) -> [a]
adjustHead [] _ = []
adjustHead (x:xs) adjuster = adjuster x : xs

regAdjust' :: State -> String -> (Bits -> Bits) -> State
regAdjust' state name adjuster = regAdjust state (regTable M.! name) adjuster

data State = State {
    programStack :: Bits,
    argumentStack :: Bits,
    callStack :: Bits,
    registerStack :: [Registers],
    program :: Bits,
    inputQueue :: Bits,
    outputQueue :: Bits
} | Terminated Bits
    deriving (Show,Eq)

loadProgram :: String -> State
loadProgram prog = State {
        programStack = [],
        argumentStack = [],
        callStack = [],
        registerStack = [initRegisters],
        program = assemble prog,
        inputQueue = [],
        outputQueue = []
    }

assemble :: String -> Bits
assemble prog = concatMap parseWord $ words prog where
    parseWord :: String -> Bits
    parseWord (sigil:word) = case sigil of
        '$' -> regTable M.! word
        'b' -> toB word
        'i' -> iToB $ read word
        '.' -> instTable M.! word

writeInput :: State -> Char -> State
writeInput state char = state { inputQueue = input } where
    input = toBits [char] ++ inputQueue state

readOutput :: State -> (String, State)
readOutput t@(Terminated _) = ("",t)
readOutput state = (fromBits $ take len output, state') where
    output = outputQueue state
    len = (length output `div` 8) * 8
    state' = state { outputQueue = drop len output }

runProgram :: State -> IO Bits
runProgram state = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    
    Terminated returnCode <- untilM isTerminated runState state
    return returnCode

isTerminated :: State -> Bool
isTerminated (Terminated _) = True
isTerminated _ = False

runState :: State -> IO State
runState state = do
    state' <- whileMM (const $ hReady stdin)
        (\s -> writeInput s <$> getChar) state
    
    let (output,state'') = readOutput $ nextState state'
    putStr output >> hFlush stdout
    return state''

nextState :: State -> State
nextState state = rJump state' (arity' + iSize) where
    state' = (iFunc inst) args state
    (inst,iSize) = instruction $ drop ip (program state)
    
    args :: [Bits]
    args = splitPlaces arity $ take arity'
        $ drop iSize $ drop ip $ program state
    
    arity' = sum arity
    arity = iArity inst
    ip = bToI $ regGet' state "ip"

takeFromIp :: State -> Int -> Bits
takeFromIp state i = take i offset where
    offset = drop ip $ program state
    ip = bToI $ regGet' state "ip"

instruction :: [Bool] -> (Instruction,Int)
instruction program = (inst M.!) &&& length
    $ head $ filter (`M.member` inst) $ inits program

rJump :: State -> Int -> State
rJump state i = regAdjust' state "ip" (+ (iToB i))

bToI :: [Bool] -> Int
bToI bits = sum $ zipWith ((*) . fromEnum) (reverse bits) $ iterate (*2) 1

iToB :: Int -> [Bool]
iToB n = [ n `div` (2 ^ x) `mod` 2 == 1 | x <- reverse powers ] where
    powers = [ 0 .. floor $ logBase 2 $ fromIntegral n ]

instance Num [Bool] where
    x + y = iToB $ (bToI x) + (bToI y)
    x * y = iToB $ (bToI x) * (bToI y)
    abs (b:bits) = True : bits
    signum bits@(b:_)
        | all not bits = [ False ]
        | otherwise = [ b ]
    fromInteger i = iToB $ fromInteger i

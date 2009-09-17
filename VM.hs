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

type Bits = [Bool]

data Instruction = Instruction {
    iArity :: Int,
    iFunc :: Bits -> State -> State
}

instTable :: M.Map String Bits
instTable = M.fromList $ map fst instDefs

inst :: M.Map Bits Instruction
inst = M.fromList $ map (first snd) instDefs

instDefs :: [ ((String, [Bool]), Instruction) ]
instDefs = [
        (("mov", toB "00"), Instruction {
            iArity = 12,
            iFunc = \bits state -> state
        }),
        (("exit", toB "1111"), Instruction {
            iArity = 0,
            iFunc = \_ _ -> Terminated
        })
    ]

type Register = Bits
-- pair up some words with the 6-digit binary counting set
regTable :: M.Map String Register
regTable = M.fromList
    $ zip ("ip" : (map (:[]) $ ['a' .. 'z'] ++ ['α' .. 'ω']))
    $ replicateM 6 [False,True]

type Registers = M.Map Register Bits

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
regAdjust state reg adjuster = state {
        registerStack = (M.adjust adjuster reg $ head $ registerStack state)
            : (tail $ registerStack state)
    }

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
} | Terminated

writeInput :: State -> Char -> State
writeInput state char = state { inputQueue = input } where
    input = toBits [char] ++ inputQueue state

readOutput :: State -> (String, State)
readOutput state = (fromBits $ take len output, state') where
    output = outputQueue state
    len = (length output `div` 8) * 8
    state' = state { outputQueue = drop len output }

runProgram :: State -> IO ()
runProgram state = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    
    untilM_ isTerminated runState state where
        isTerminated Terminated = True
        isTerminated _ = False

runState :: State -> IO State
runState state = do
    state' <- whileMM (const $ hReady stdin)
        (\s -> writeInput s <$> getChar) state
    
    let (output,state'') = readOutput state'
    putStr output >> hFlush stdout
    return state''

nextState :: State -> State
nextState state = rJump state arity where
    inst = instruction $ drop ip (program state)
    arity = iArity inst
    ip = bToI $ regGet' state "ip"

instruction :: [Bool] -> Instruction
instruction program = inst M.! i where
    i = head $ filter (`M.member` inst) $ inits program

rJump :: State -> Int -> State
rJump state i = regAdjust' state "ip" (+ (iToB i))

bToI :: [Bool] -> Int
bToI bits = sum $ zipWith ((*) . fromEnum) (reverse bits) $ iterate (*2) 1

iToB :: Int -> [Bool]
iToB n = [ n `div` (2 ^ x) == 1 | x <- reverse powers ] where
    powers = [ 0 .. floor $ logBase 2 $ fromIntegral n ]

instance Num [Bool] where
    x + y = iToB $ (bToI x) + (bToI y)
    x * y = iToB $ (bToI x) * (bToI y)
    abs (b:bits) = True : bits
    signum bits@(b:_)
        | all not bits = [ False ]
        | otherwise = [ b ]
    fromInteger i = iToB $ fromInteger i

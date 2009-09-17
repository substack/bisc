{-
    BISC - the bit-vector instruction set
        sounds like: bisque
        rhymes with: risk, disk, whisk, brisk, elektrisk
    
    Features:
        * All instructions are bit aligned for better huffman coding! Save every
          bit! Except for all the bits wasted out of laziness.
          
        * Registers are infinite bit vectors! This would make this instruction
          set hard/impossible to implement in hardware probably. That is
          somebody else's problem.
        
        * There is a stack of registers to create lexical closures! The
          programmer need not worry about a function call's side effects on the
          registers. Proper isolation!
          
        * Instructions can have dynamic arity! With precision to the bit!
          It would be trivial to allow instructions to modify the instruction
          table at runtime.
    
    Caveats:
        * It's really complex. It'd be much worse in C. Blame the deadline for
          some of the ugliness.
        
        * The implementation is very fargone and abstracted away from any actual 
          raw bit twiddling. This is arguably a good thing!
        
        * Modern optimizations would be really hard to implement. Old
          optimizations for non-Von Neumann architectures will probably work
          well enough however.
-}

{-# LANGUAGE FlexibleInstances #-}
module Main where

import VM.Util

import Data.Bits (xor)
import qualified Data.Map as M
import Control.Arrow
import System.IO
import Control.Monad
import Data.BitString
import Control.Applicative
import Data.Foldable (toList)
import Data.List (inits,intersperse)
import Data.List.Split (splitPlaces,splitEvery)
import System.IO.Unsafe
import System.Environment (getArgs)

type Bits = [Bool]

data Instruction = Instruction {
    iArity :: [Int],
    iFunc :: [Bits] -> State -> State
}


main :: IO Int
main = mainArgs =<< getArgs

-- to call main from interactive console with arguments
mainArgs :: [String] -> IO Int
mainArgs args = do
    when (null args) $ error "Usage: bisc [filename.asm]"
    
    state <- loadProgram <$> readFile (head args)
    let -- wrap long lines and indent to make the bytecode output look nice
        wrap cols indent =
            concatMap (++ (replicate indent ' '))
            . intersperse "\n"
            . ("" :)
            . splitEvery (cols - indent)
        bits = fromB $ program state
   
    putStrLn $ "Bytecode: "
        ++ (show $ length bits) ++ " bits, "
        ++ (show $ (fromIntegral $ length bits) / 8) ++ " bytes, "
        ++ (wrap 80 4 bits) ++ "\n"
    
    code <- runProgram state
    putStrLn "Return value:"
    putStrLn $ "    Binary: " ++ (wrap 80 8 $ fromB code) ++ "\n"
    putStrLn $ "    Integer: " ++ show (bToI code) ++ "\n"
    return $ bToI code

instTable :: M.Map String Bits
instTable = M.fromList $ map fst instDefs

inst :: M.Map Bits Instruction
inst = M.fromList $ map (first snd) instDefs

instDefs :: [ ((String, [Bool]), Instruction) ]
instDefs = [
        -- copy a register's contents into another register
        (("mov", toB "000"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ regGet state src
        }),
        -- load an integer into a register
        (("loadI", toB "00100"), Instruction {
            iArity = [6,32],
            iFunc = \[dst,bits] state ->
                regSet state dst bits
        }),
        -- load a variable amount with integer size into a register
        (("loadVI", toB "00101"), Instruction {
            iArity = [6,32],
            iFunc = \[dst,size] state ->
                let bits = takeFromIp state $ bToI size
                in (flip rJump $ bToI size) $ regSet state dst bits
        }),
        -- load a byte into a register
        (("loadB", toB "00110"), Instruction {
            iArity = [6,8],
            iFunc = \[dst,bits] state ->
                regSet state dst bits
        }),
        -- load a variable amount with byte size into a register
        (("loadVB", toB "00111"), Instruction {
            iArity = [6,8],
            iFunc = \[dst,size] state ->
                let bits = takeFromIp state $ bToI size
                in (flip rJump $ bToI size) $ regSet state dst bits
        }),
        -- add two registers, storing the result in another register
        (("add", toB "01000"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ (regGet state x) + (regGet state y)
        }),
        -- add a byte to a register, modifying the value of that register
        (("addB", toB "01001"), Instruction {
            iArity = [6,8],
            iFunc = \[dst,x] state ->
                regAdjust state dst (+ x)
        }),
        -- subtract two registers, storing the result in another register
        (("sub", toB "01010"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ (regGet state x) - (regGet state y)
        }),
        -- subtract a byte from a register
        (("subB", toB "01011"), Instruction {
            iArity = [6,8],
            iFunc = \[dst,x] state ->
                regSet state dst $ iToB
                    $ (bToI $ regGet state dst) - bToI x
        }),
        -- multiply two registers, storing the result in another register
        (("mul", toB "01100"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ iToB
                    $ (bToI $ regGet state x) * (bToI $ regGet state y)
        }),
        -- multiply a register by a byte
        (("mulB", toB "01101"), Instruction {
            iArity = [6,8],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ iToB
                    $ (bToI $ regGet state x) * (bToI y)
        }),
        -- divide two registers, storing the result in another register
        (("div", toB "01110"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ iToB
                    $ (bToI $ regGet state x) `div` (bToI $ regGet state y)
        }),
        -- divide a register by a byte
        (("divB", toB "01111"), Instruction {
            iArity = [6,8],
            iFunc = \[dst,x] state ->
                regSet state dst $ iToB
                    $ (bToI $ regGet state dst) `div` (bToI x)
        }),
        -- absolute integer equality jump
        (("jeq", toB "1000"), Instruction {
            iArity = [32,6,6],
            iFunc = \[dst,x,y] state ->
                if (regGet state x) == (regGet state y)
                    then aJump state $ bToI dst
                    else state
        }),
        -- relative byte equality jump
        (("rjeqB", toB "1001"), Instruction {
            iArity = [8,6,6],
            iFunc = \[offset,x,y] state ->
                if regGet state x == regGet state y
                    then rJump state $ bToI offset
                    else state
        }),
        -- variable-length integer-size absolute equality jump
        (("ajeqVI", toB "10100"), Instruction {
            iArity = [6,6,32],
            iFunc = \[x,y,size] state ->
                let addr = bToI $ takeFromIp state $ bToI size
                in (flip rJump $ bToI size) $
                    if regGet state x == regGet state y
                        then rJump state addr else state
        }),
        -- bitwise "and" of two registers, storing the result in a third
        (("and", toB "101010"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    zipWith (&&) (regGet state x) (regGet state y)
        }),
        -- bitwise "or" of two registers, storing the result in a third
        (("or", toB "101011"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    zipWith (||) (regGet state x) (regGet state y)
        }),
        -- bitwise "xor" of two registers, storing the result in a third
        (("xor", toB "101100"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    zipWith (\x y -> (not x && y) || (x && not y))
                        (regGet state x) (regGet state y)
        }),
        -- bitwise "nand" of two registers, storing the result in a third
        (("nand", toB "101100"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    zipWith (\x y -> not (x && y))
                        (regGet state x) (regGet state y)
        }),
        -- compute the number of bits used in a register
        (("length", toB "111100"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ iToB $ length $ regGet state src
        }),
        -- truncate a register
        (("truncateI", toB "111101"), Instruction {
            iArity = [6,32],
            iFunc = \[dst,size] state ->
                regAdjust state dst (take $ bToI size)
        }),
        -- exit with the value of a register
        (("exitR", toB "111110"), Instruction {
            iArity = [6],
            iFunc = \[x] state ->
                Terminated $ regGet state x
        }),
        -- exit with an integer status
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
regSet state reg bits = regAdjust state reg (const bits)

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
assemble prog = concatMap parseWord $ words
    $ concatMap stripComments $ lines prog where
        stripComments :: String -> String
        stripComments line = ' ' : takeWhile (/= ';') line
        
        parseWord :: String -> Bits
        parseWord (sigil:word) = case sigil of
            -- $ : register
            '$' -> regTable M.! word
            -- b : binary string
            'b' -> toB word
            -- i : 32-bit integer
            'i' -> reverse $ take 32 $ reverse
                $ (replicate 32 False) ++ (iToB $ read word)
            -- . : instruction
            '.' -> instTable M.! word
            -- * : label
            '*' -> undefined
            -- & : address of label
            '&' -> undefined

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
nextState state = state' where
    state' = (iFunc inst) args $ rJump state (arity' + iSize)
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

aJump :: State -> Int -> State
aJump state i = regSet' state "ip" $ iToB i

bToI :: [Bool] -> Int
bToI bits = sum $ zipWith ((*) . fromEnum) (reverse bits) $ iterate (*2) 1

iToB :: Int -> [Bool]
iToB n = [ n `div` (2 ^ x) `mod` 2 == 1 | x <- reverse powers ] where
    powers = [ 0 .. floor $ logBase 2 $ fromIntegral n ]

instance Num [Bool] where
    x + y = iToB $ (bToI x) + (bToI y)
    x - y = iToB $ (bToI x) - (bToI y)
    x * y = iToB $ (bToI x) * (bToI y)
    abs (b:bits) = True : bits
    signum bits@(b:_)
        | all not bits = [ False ]
        | otherwise = [ b ]
    fromInteger i = iToB $ fromInteger i

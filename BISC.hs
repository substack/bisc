{-# LANGUAGE FlexibleInstances #-}
module BISC where

import BISC.Util

import System.IO hiding (putStr)
import Prelude hiding (putStr)
import System.IO.UTF8 (putStr)

import Data.Bits (xor)
import qualified Data.Map as M
import Control.Arrow
import Control.Monad
import Data.BitString
import Control.Applicative
import Data.List (inits,mapAccumL)
import Data.List.Split (splitPlaces)

type Bits = [Bool]

data Instruction = Instruction {
    iArity :: [Int],
    iFunc :: [Bits] -> State -> State
}

-- instruction tables and such
instTable :: M.Map String Bits
instTable = M.fromList $ map fst instDefs

inst :: M.Map Bits Instruction
inst = M.fromList $ map (first snd) instDefs

-- the instruction definitions
instDefs :: [ ((String, [Bool]), Instruction) ]
instDefs = [
        -- copy a register's contents into another register
        (("mov", toB "0000"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ regGet state src
        }),
        -- copy an integer number of bits from one register to another
        (("movI", toB "0001"), Instruction {
            iArity = [6,6,32],
            iFunc = \[dst,src,size] state ->
                regSet state dst $ take (bToI size) $ regGet state src
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
        (("jeq", toB "10000"), Instruction {
            iArity = [32,6,6],
            iFunc = \[dst,x,y] state ->
                if (bToI $ regGet state x) == (bToI $ regGet state y)
                    then aJump state $ bToI dst
                    else state
        }),
        -- absolute integer negative equality jump
        (("jne", toB "10001"), Instruction {
            iArity = [32,6,6],
            iFunc = \[dst,x,y] state ->
                if (bToI $ regGet state x) /= (bToI $ regGet state y)
                    then aJump state $ bToI dst
                    else state
        }),
        -- absolute integer less than jump
        (("jlt", toB "10010"), Instruction {
            iArity = [32,6,6],
            iFunc = \[dst,x,y] state ->
                if (bToI $ regGet state x) < (bToI $ regGet state y)
                    then aJump state $ bToI dst
                    else state
        }),
        -- absolute integer greater than jump
        (("jgt", toB "10011"), Instruction {
            iArity = [32,6,6],
            iFunc = \[dst,x,y] state ->
                if (bToI $ regGet state x) > (bToI $ regGet state y)
                    then aJump state $ bToI dst
                    else state
        }),
        -- unconditional absolute integer jump
        (("jmp", toB "101000"), Instruction {
            iArity = [32],
            iFunc = \[dst] state -> aJump state $ bToI dst
        }),
        -- bitwise "not" of one register, storing the result in a second
        (("not", toB "101001"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ map not (regGet state src)
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
        (("nand", toB "101101"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    zipWith (\x y -> not (x && y))
                        (regGet state x) (regGet state y)
        }),
        -- left shift a register by a register, storing the result in a third
        (("shiftL", toB "101110"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $ (regGet state y)
                    ++ (replicate (bToI $ regGet state x)) False
        }),
        -- right shift a register by a register, storing the result in a third
        (("shiftR", toB "101111"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    let r = regGet state x
                    in take (length r - bToI r) $ regGet state y
        }),
        -- arithmetic right shift a register by a register, storing the result
        -- in a third register
        -- The first bit is treated as the sign bit not that there is any
        -- explicitly signd arithmetic.
        (("shiftRA", toB "110000"), Instruction {
            iArity = [6,6,6],
            iFunc = \[dst,x,y] state ->
                regSet state dst $
                    let
                        r = regGet state x
                        (sign:[],v) = splitAt 1 $ regGet state y
                    in sign : take (length r - bToI r) v
        }),
        -- compute the number of bits used in a register
        (("length", toB "111100"), Instruction {
            iArity = [6,6],
            iFunc = \[dst,src] state ->
                regSet state dst $ iToB $ length $ regGet state src
        }),
        -- truncate a register
        (("truncateVI", toB "111101"), Instruction {
            iArity = [6,32],
            iFunc = \[dst,size] state ->
                regAdjust state dst (take $ bToI size)
        }),
        -- block until one byte is available in the out register
        (("blockB", toB "1111110"), Instruction {
            iArity = [],
            iFunc = \[] state ->
                if (length $ regGet' state "in") >= 8
                    then state
                    else rJump state 8
        }), -- block until the integer-sized number of bits are available
        (("blockVI", toB "1111111"), Instruction {
            iArity = [32],
            iFunc = \[size] state ->
                if (length $ regGet' state "in") >= bToI size
                    then state
                    else rJump state (- length "1111111")
        }),
        -- exit with the value of a register
        (("exitR", toB "1111100"), Instruction {
            iArity = [6],
            iFunc = \[x] state ->
                Terminated $ regGet state x
        }),
        -- exit with an integer status
        (("exitI", toB "1111101"), Instruction {
            iArity = [32],
            iFunc = \[bits] _ -> Terminated bits
        })
    ]

type Register = Bits
-- pair up some words with the 6-digit binary counting set
regTable :: M.Map String Register
regTable = M.fromList
    -- ip: instruction pointer
    -- in: input buffer
    -- out: output buffer
    -- roman letters after that, then greek letters
    $ zip ("ip" : "in" : "out" : (map (:[]) $ ['a' .. 'z'] ++ ['α' .. 'ω']))
    $ replicateM 6 [False,True] -- this line is magical

type Registers = M.Map Register Bits
initRegisters :: Registers
initRegisters = M.fromList $ zip (M.elems regTable) $ repeat []

-- get and set registers...

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

-- state of the simulated system
data State = State {
    -- most fields are not yet useful on account of time constraints
    programStack :: Bits,
    argumentStack :: Bits,
    callStack :: Bits,
    registerStack :: [Registers],
    program :: Bits
} | Terminated Bits
    deriving (Show,Eq)

-- load a program by assembling a string into a state machine
loadProgram :: String -> State
loadProgram prog = State {
        programStack = [],
        argumentStack = [],
        callStack = [],
        registerStack = [initRegisters],
        program = assemble prog
    }

-- turn a string program into a series of bits
assemble :: String -> Bits
assemble prog = concat $ snd $ mapAccumL parseWord (M.empty,0) $ words
    $ concatMap stripComments $ lines prog where
        stripComments :: String -> String
        stripComments line = ' ' : takeWhile (/= ';') line
        
        parseWord ::
            (M.Map String Int, Int) ->
            String ->
            ((M.Map String Int, Int), Bits)
        parseWord (labels,pos) (sigil:word) = parsed where
            parsed = ((labels', pos + length bits), bits)
            (labels',bits) = case sigil of
                -- $ : register
                '$' -> (labels, regTable M.! word)
                -- b : binary string
                'b' -> (labels, toB word)
                -- B : 8-bit byte
                'B' -> (labels, pack 8 $ iToB $ read word)
                -- c: 8-bit byte (character order)
                'c' -> (labels, reverse $ pack 8 $ iToB $ read word)
                -- i : 32-bit integer
                'i' -> (labels, pack 32 $ iToB $ read word)
                -- " : ascii string
                '"' -> (labels, toBits word)
                -- . : instruction
                '.' -> (labels, instTable M.! word)
                -- * : label
                '*' -> (M.insert word pos labels, [])
                -- & : address of label
                '&' -> (labels, pack 32 $ iToB $ labels M.! word)

-- stuff a value into a finite bit vector
pack :: Int -> Bits -> Bits
pack n bits = reverse $ take n $ reverse
    $ (replicate n False) ++ bits

-- add a byte to the $in register
writeInput :: State -> Char -> State
writeInput state char = regSet' state "in" input where
    input = toBits [char] ++ regGet' state "in"

-- take bytes out of the $out register
readOutput :: State -> (String, State)
readOutput t@(Terminated _) = ("",t)
readOutput state = (fromBits $ take len output, state') where
    output = regGet' state "out"
    len = (length output `div` 8) * 8
    state' = regSet' state "out" $ drop len output

-- run the program until it halts or doesn't
runProgram :: State -> IO Bits
runProgram state = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    
    Terminated returnCode <- untilM isTerminated runState state
    return returnCode

-- whether the program has finished
isTerminated :: State -> Bool
isTerminated (Terminated _) = True
isTerminated _ = False

-- thread the state along, adjusting input and output buffers
runState :: State -> IO State
runState state = do
    state' <- whileMM (const $ hReady stdin)
        (\s -> writeInput s <$> getChar) state
    
    let (output,state'') = readOutput $ nextState state'
    putStr output >> hFlush stdout
    return state''

-- move from one state to the next,
-- giving instructions their requested operands
-- and advancing the instruction pointer
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

-- take bits which come after the instruction pointer
takeFromIp :: State -> Int -> Bits
takeFromIp state i = take i offset where
    offset = drop ip $ program state
    ip = bToI $ regGet' state "ip"

-- match a program bit vector with the first instruction code that matches
instruction :: [Bool] -> (Instruction,Int)
instruction program = (inst M.!) &&& length
    $ head $ filter (`M.member` inst) $ inits program

-- relative jump
rJump :: State -> Int -> State
rJump state i = regAdjust' state "ip" (+ (iToB i))

-- absolute jump
aJump :: State -> Int -> State
aJump state i = regSet' state "ip" $ iToB i

-- convert a bit vector to an integer
bToI :: [Bool] -> Int
bToI bits = sum $ zipWith ((*) . fromEnum) (reverse bits) $ iterate (*2) 1

-- convert an integer to a bit vector
iToB :: Int -> [Bool]
iToB n = [ n `div` (2 ^ x) `mod` 2 == 1 | x <- reverse powers ] where
    powers = [ 0 .. floor $ logBase 2 $ fromIntegral n ]

-- do some bit vector mathematics
instance Num [Bool] where
    x + y = iToB $ (bToI x) + (bToI y)
    x - y = iToB $ (bToI x) - (bToI y)
    x * y = iToB $ (bToI x) * (bToI y)
    abs (b:bits) = True : bits
    signum bits@(b:_)
        | all not bits = [ False ]
        | otherwise = [ b ]
    fromInteger i = iToB $ fromInteger i

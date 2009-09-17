module VM where

import VM.Util

import qualified Data.Map as M
import Control.Arrow
import System.IO
import Control.Monad
import Data.BitString

type Bits = [Bool]

data Instruction = Instruction {
    iArity :: Int,
    iFunc :: Bits -> State -> State
}

type InstructionNames = M.Map String Bits
type Registers = M.Map Bits Bits
type RegisterNames = M.Map String Bits

instructionTable :: M.Map String Bits
instructionTable = M.fromList $ map fst instructionDefs

instructions :: M.Map Bits Instruction
instructions = M.fromList $ map (first snd) instructionDefs

instructionDefs :: [ ((String, [Bool]), Instruction) ]
instructionDefs = [
        (("mov", toB "00"), Instruction {
            iArity = 4,
            iFunc = (\bits state -> state)
        })
    ]

data State = State {
    programStack :: Bits,
    argumentStack :: Bits,
    callStack :: Bits,
    registerStack :: [Registers],
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
        (\s -> writeInput s `liftM` getChar) state
    
    let (output,state'') = readOutput state'
    putStr output >> hFlush stdout
    return state''
    
nextState :: State -> State
nextState state = undefined

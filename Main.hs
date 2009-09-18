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
          some of the ugliness. As an aside, Hindley-Milner type inference is
          really tremendously useful when putting together big projects in a
          short amount of time.
        
        * The implementation is very fargone and abstracted away from any actual 
          raw bit twiddling. This is arguably a good thing!
        
        * Modern optimizations would be really hard to implement. Old
          optimizations for non-Von Neumann architectures will probably work
          well enough however.
-}

module Main where

import BISC
import Data.BitString

import Control.Monad
import Control.Applicative
import Data.List (intersperse)
import Data.List.Split (splitEvery)
import System.Environment (getArgs)

-- the regular interface
main :: IO Int
main = mainArgs =<< getArgs

-- and this interface to set the arguments directly from the interactive console
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
    
    putStrLn "Program output:"
    code <- runProgram state
    
    putStrLn "\nReturn value:"
    putStrLn $ "    Binary: " ++ (wrap 80 8 $ fromB code) ++ "\n"
    putStrLn $ "    Integer: " ++ show (bToI code) ++ "\n"
    return $ bToI code

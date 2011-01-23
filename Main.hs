module Main where

import BISC
import Data.BitString

import Control.Monad
import Control.Applicative
import Data.List (intersperse)
import Data.List.Split (splitEvery)
import System.Environment (getArgs)

import Prelude hiding (readFile)
import System.IO.UTF8 (readFile)

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
   
    putStrLn $ "Bitcode: "
        ++ (show $ length bits) ++ " bits, "
        ++ (show $ (fromIntegral $ length bits) / 8) ++ " bytes, "
        ++ (wrap 80 4 bits) ++ "\n"
    
    putStrLn "Program output:"
    code <- runProgram state
    
    putStrLn "\nReturn value:"
    putStrLn $ "    Binary: " ++ (wrap 80 8 $ fromB code) ++ "\n"
    putStrLn $ "    Integer: " ++ show (bToI code) ++ "\n"
    return $ bToI code

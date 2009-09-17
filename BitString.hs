module Data.BitString where
import Data.Char (chr,ord)
import Data.List.Split (splitEvery)

toBits :: String -> [Bool]
toBits = concatMap toBits' where
    toBits' :: Char -> [Bool]
    toBits' num = fst $ foldl place ([],ord num) [7,6..0] where
        place :: ([Bool],Int) -> Int -> ([Bool],Int)
        place (bits,num) power = if num >= 2 ^ power
            then (True:bits, num - 2 ^ power)
            else (False:bits, num)

fromBits :: [Bool] -> String
fromBits = map fromBits' . splitEvery 8 where
    fromBits' :: [Bool] -> Char
    fromBits' bits = chr $ sum $ zipWith (*) twos bitString where
        twos = iterate (2*) 1
        bitString = map fromEnum bits

fromB :: [Bool] -> String
fromB = map (\bit -> if bit then '1' else '0')

toB :: String -> [Bool]
toB = map (== '1')

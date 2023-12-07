module Utils (spanOn, splitOn, strip) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, findIndex)

spanOn :: (a -> Bool) -> [a] -> ([a], [a])
spanOn test arr = let (begin, end) = maybe (arr, []) (`splitAt` arr) $ findIndex test arr in (begin, dropWhile test end)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn test arr = let (begin, end) = spanOn test arr in begin : splitOn test end

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
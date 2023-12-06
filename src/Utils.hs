module Utils (spanOn, splitOn) where

import Data.List (findIndex)

spanOn :: (a -> Bool) -> [a] -> ([a], [a])
spanOn test arr = let (begin, end) = maybe (arr, []) (`splitAt` arr) $ findIndex test arr in (begin, dropWhile test end)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn test arr = let (begin, end) = spanOn test arr in begin : splitOn test end
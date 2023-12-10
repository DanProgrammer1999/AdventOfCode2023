module Solution where

import Control.Applicative

-- runAlgorithm Day1.Part1

-- instance Solution Part1 where
--  inputFile = "day1.txt"

inputPath :: (Solution a) => Const FilePath a
inputPath = fmap id inputFile

class Solution a where
  inputFile :: Const String a

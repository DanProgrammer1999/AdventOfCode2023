{-# LANGUAGE LambdaCase #-}

module Day9 where

import Data.Maybe (listToMaybe)

parseLine :: String -> [Int]
parseLine = map read . words

lineDifference :: [Int] -> [Int]
lineDifference = \case
  [] -> []
  (num : nums) -> if all (== 0) (num : nums) then [] else zipWith (-) (num : nums) nums

getLineDifferences :: [[Int]] -> [[Int]]
getLineDifferences [] = []
getLineDifferences (l : ls) = let nextStep = lineDifference l in if null nextStep then ls else getLineDifferences (nextStep : l : ls)

extrapolateFromDifferences :: [[Int]] -> Int
extrapolateFromDifferences = maybe 0 sum . mapM listToMaybe

part1 :: String -> Int
part1 = sum . map (extrapolateFromDifferences . getLineDifferences . (: []) . reverse . parseLine) . lines

part2 :: String -> Int
part2 = sum . map (extrapolateFromDifferences . getLineDifferences . (: []) . parseLine) . lines
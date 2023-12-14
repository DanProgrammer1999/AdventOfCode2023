module Day13 where

import Control.Monad (liftM2)
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Utils (splitOn)

type RockPattern = [String]

type RowIndex = Int

type ColIndex = Int

toMaybe :: Bool -> a -> Maybe a
toMaybe True = Just
toMaybe False = const Nothing

parseInput :: String -> [RockPattern]
parseInput input = splitOn null $ lines input

calculateSmudges :: RockPattern -> [Int]
calculateSmudges [] = []
calculateSmudges pattern = allSmudges
  where
    lineDifferences a b = length $ filter id $ zipWith (/=) a b
    allSmudges = map (\i -> let (left, right) = splitAt i pattern in sum (zipWith lineDifferences (reverse left) right)) [1 .. length pattern]

findRockPatternMirror :: Int -> RockPattern -> [Either RowIndex ColIndex]
findRockPatternMirror smudgeCount = liftM2 (++) (map Left . findMirrorLines) (map Right . findMirrorLines . transpose)
  where
    findMirrorLines = map fst . filter ((== smudgeCount) . snd) . zip [0 ..] . calculateSmudges

mirrorValue :: Either RowIndex ColIndex -> Int
mirrorValue (Left rowIdx) = (rowIdx + 1) * 100
mirrorValue (Right colIdx) = colIdx + 1

-- part1 :: String -> Int
part1 :: String -> Int
part1 = sum . map mirrorValue . concatMap (findRockPatternMirror 0) . parseInput

-- part2 :: String -> Int
part2 = sum . map mirrorValue . concatMap (findRockPatternMirror 1) . parseInput
module Day6 where

import Control.Applicative (Applicative (liftA2))
import Data.Char (isDigit)
import Data.List (stripPrefix)
import Utils (strip)

-- Parsing
parseLine :: String -> String -> Maybe [Int]
parseLine prefix line = map read . words <$> stripPrefix prefix (strip line)

parseInput1 :: [String] -> Maybe [(Int, Int)]
parseInput1 [timeLine, distanceLine] = liftA2 zip (parseLine "Time:" timeLine) (parseLine "Distance:" distanceLine)
parseInput1 _ = Nothing

parseInput2 :: [String] -> Maybe (Int, Int)
parseInput2 [timeLine, distanceLine] = Just (read (filter isDigit timeLine), read (filter isDigit distanceLine))
parseInput2 _ = Nothing

-- Algorithms

-- Part 1
-- distance = x * (time - x) where x is number of ms holding charge button
-- time gives maximum distance where distance' = 0 (derivative) ==> x = t/2 ==> max d = t^2 / 4

quadraticRoots :: (Floating a, Ord a) => a -> a -> a -> [a]
quadraticRoots a b c
  | d >= 0 = ((-b - sqrt d) / 2) : ([(-b + sqrt d) / 2 | d > 0])
  | otherwise = []
  where
    d = b ^ 2 - 4 * a * c

waysToBeatRecord :: (Int, Int) -> Int
waysToBeatRecord (time, currentBest) = case bestDistanceTimes of
  [minTime, maxTime] -> (ceiling maxTime - 1) - (floor minTime + 1) + 1
  [_] -> 0
  [] -> time
  _ -> 0
  where
    -- d = x*t - x^2 ==> x^2 - x*t + d = 0 ==> a = 1, b = -t, c = d
    bestDistanceTimes = quadraticRoots 1 (fromIntegral (-time)) (fromIntegral currentBest)

part1 :: [String] -> Maybe Int
part1 = fmap (product . map waysToBeatRecord) . parseInput1

part2 :: [String] -> Maybe Int
part2 = fmap waysToBeatRecord . parseInput2
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day5 (part1, part2) where

import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Utils

data Range = Range {minRange :: Int, maxRange :: Int, delta :: Int} deriving (Show, Eq)

instance Ord Range where
  compare (Range r1 _ _) (Range r2 _ _) = compare r1 r2

type RangeMap = [Range]

type Seeds = [Int]

parseRange :: String -> Maybe (Int, Int, Int)
parseRange line = case map read $ splitOn (== ' ') line of
  [destinationRangeStart, sourceRangeStart, rangeLength] -> Just (destinationRangeStart, sourceRangeStart, rangeLength)
  _ -> Nothing

convertToRange :: (Int, Int, Int) -> Range
convertToRange (destinationRangeStart, sourceRangeStart, rangeLength) =
  Range sourceRangeStart (sourceRangeStart + rangeLength - 1) (destinationRangeStart - sourceRangeStart)

parseGroup :: [String] -> Maybe RangeMap
parseGroup [] = Nothing
parseGroup (_ : group) = mapM (fmap convertToRange . parseRange) group

parseSeeds :: String -> Maybe [Int]
parseSeeds ('s' : 'e' : 'e' : 'd' : 's' : ':' : ' ' : seedLine) = Just $ map read $ words seedLine
parseSeeds _ = Nothing

parseInput :: [String] -> Maybe (Seeds, [RangeMap])
parseInput input = case splitOn (== "") input of
  ([seedLine] : groups) -> liftA2 (,) (parseSeeds seedLine) (mapM parseGroup groups)
  _ -> Nothing

mergeRanges :: RangeMap -> RangeMap -> RangeMap
mergeRanges range1 [] = range1
mergeRanges [] range2 = range2
mergeRanges (r1 : rs1) (r2 : rs2)
  | maxRange r1 < minRange r2 = r1 : mergeRanges rs1 (r2 : rs2)
  | minRange r1 <= minRange r2 && maxRange r1 <= maxRange r2 = handleIntersectingRanges r1 r2
  | minRange r1 < minRange r2 && maxRange r2 < maxRange r1 = handleOverlappingRanges r1 r2
  | otherwise = mergeRanges (r2 : rs2) (r1 : rs1)
  where
    handleIntersectingRanges (Range a b d1) (Range c d d2) =
      [Range a (c - 1) d1 | a < c]
        ++ [Range c b (d1 + d2)]
        ++ mergeRanges rs1 ([Range (c + 1) d d2 | c < d] ++ rs2)

    handleOverlappingRanges (Range a b d1) (Range c d d2) = Range a (c - 1) d1 : Range c d (d1 + d2) : mergeRanges (Range (d + 1) b d1 : rs1) rs2

applyRange :: Int -> RangeMap -> Int
applyRange seed [] = seed
applyRange seed (range : rs) =
  if seed >= minRange range && seed <= maxRange range
    then seed + delta range
    else applyRange seed rs

part1 :: [String] -> Maybe Int
part1 input = fmap minimum $ parseInput input >>= \(seeds, ranges) -> Just $ map (\seed -> foldl applyRange seed ranges) seeds

part2 :: [String] -> Int
part2 input = minimum $ map (\seed -> foldl applyRange seed ranges) pairedSeeds
  where
    (seeds, ranges) = fromMaybe ([], []) $ parseInput input
    pairSeeds (start : len : rest) = [start .. start + len - 1] : pairSeeds rest
    pairSeeds _ = []

    pairedSeeds = concat $ pairSeeds seeds

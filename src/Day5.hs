module Day5 where

import Control.Applicative (liftA2)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Utils

data Range = Range {minRange :: Int, maxRange :: Int, delta :: Int} deriving (Eq)

instance Show Range where
  show range = "[" ++ show (minRange range) ++ "-" ++ show (maxRange range) ++ ": " ++ show (delta range) ++ "]"

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

-- parseInput' :: [String] -> String
parseInput' input = intercalate "\n" $ map show $ buildRangeMap $ snd $ fromMaybe ([], []) (parseInput input)
  where
    transformRangeMap =
      intercalate ",\t"
        . map
          ( \(Range minRange maxRange delta) ->
              "("
                ++ show minRange
                ++ ", "
                ++ show maxRange
                ++ ") -> ("
                ++ show (minRange + delta)
                ++ ", "
                ++ show (maxRange + delta)
                ++ ")"
          )

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

-- buildRangeMap :: [RangeMap] -> [((Int, Int), [(Int, Int)])]
buildRangeMap rangeMaps = foldr mapRanges [lastRangeSources] others
  where
    lastRangeSources = map rangeToSourceRange lastRange
    (lastRange : others) = reverse rangeMaps

rangeToSourceRange :: Range -> (Int, Int)
rangeToSourceRange (Range low high _) = (low, high)

rangeToDestinationRange :: Range -> (Int, Int)
rangeToDestinationRange (Range low high d) = (low + d, high + d)

mapRanges :: RangeMap -> [[(Int, Int)]] -> [[(Int, Int)]]
mapRanges a = map (concatMap findRanges)
  where
    findRanges (start, end) =
      [ rangeToDestinationRange $ clipRange start end range
        | range <- a,
          minRange range + delta range >= start || maxRange range + delta range <= end
      ]

    clipRange start end range = range {minRange = max (minRange range) start, maxRange = min (maxRange range) end}

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

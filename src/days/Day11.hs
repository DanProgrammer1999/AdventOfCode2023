{-# LANGUAGE FlexibleInstances #-}

module Day11 where

import Control.Monad (guard)
import Data.List (transpose)

type StarMap = [String]

type RowIndex = Int

type ColIndex = Int

newtype Coords a = Coords {getCoords :: (a, a)} deriving (Show, Eq)

instance (Num a) => Num (Coords a) where
  (Coords (a1, b1)) + (Coords (a2, b2)) = Coords (a1 + a2, b1 + b2)
  (Coords (a1, b1)) * (Coords (a2, b2)) = Coords (a1 * a2, b1 * b2)
  abs (Coords (a, b)) = Coords (abs a, abs b)
  fromInteger x = Coords (fromInteger x, fromInteger x)
  negate (Coords (a, b)) = Coords (negate a, negate b)
  signum (Coords (a, b)) = Coords (signum a, signum b)

parseInput :: String -> StarMap
parseInput = lines

indexedMatrix :: [[a]] -> [((Int, Int), a)]
indexedMatrix = concat . zipWith (\i row -> map (\(j, v) -> ((i, j), v)) row) [0 ..] . map (zip [0 ..])

findEmptyRows :: StarMap -> [Int]
findEmptyRows = map fst . filter (all (== '.') . snd) . zip [0 ..]

third :: (a, b, c) -> c
third (_, _, c) = c

distanceBetweenGalaxies :: Int -> [Int] -> [Int] -> Coords Int -> Coords Int -> Int
distanceBetweenGalaxies multiplier emptyRows emptyCols coord1 coord2 = sumCoords (abs (coord1 - coord2)) + (multiplier - 1) * allExpansions coord1 coord2
  where
    sumCoords (Coords (x, y)) = x + y
    allExpansions (Coords (x1, y1)) (Coords (x2, y2)) = expansionsBetweenCoordinates emptyRows x1 x2 + expansionsBetweenCoordinates emptyCols y1 y2
    expansionsBetweenCoordinates arr i j = length $ filter (\x -> x > min i j && x < max i j) arr

countAllDistances :: Int -> StarMap -> [Int]
countAllDistances expansionSpeed starMap = do
  let allGalaxies = map (Coords . fst) $ filter ((== '#') . snd) $ indexedMatrix starMap

  galaxy1 <- allGalaxies
  galaxy2 <- allGalaxies

  guard (getCoords galaxy2 > getCoords galaxy1)

  return $ distanceBetweenGalaxies expansionSpeed (findEmptyRows starMap) (findEmptyRows (transpose starMap)) galaxy1 galaxy2

part1 :: String -> Int
part1 = sum . countAllDistances 2 . parseInput

part2 :: String -> Int
part2 = sum . countAllDistances 1000000 . parseInput
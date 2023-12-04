{-# LANGUAGE TupleSections #-}

module Day3 where

import Data.Char (isDigit)

-- Data
----------
type RowSpan = IndexedElement Int

type TableIndex = (Int, Int)

data IndexedElement a = Idx {rowIdx :: Int, colIdx :: Int, element :: a} deriving (Show)

-- Utils
----------
listElement :: Int -> [a] -> Maybe a
listElement 0 (x : _) = Just x
listElement _ [] = Nothing
listElement i (_ : xs)
  | i < 0 = Nothing
  | otherwise = listElement (i - 1) xs

matrixElement :: [[a]] -> TableIndex -> Maybe a
matrixElement arr (i, j) = listElement i arr >>= \row -> listElement j row

matrixSubrow :: [[a]] -> RowSpan -> Maybe [a]
matrixSubrow matrix (Idx i j len) = take len . drop j <$> listElement i matrix

enumerateMatrix :: [[a]] -> [IndexedElement a]
enumerateMatrix matrix = concat $ zipWith (map . uncurry . Idx) [0 ..] $ map (zip [0 ..]) matrix

findNumberLocations :: [IndexedElement Char] -> [RowSpan]
findNumberLocations [] = []
findNumberLocations (x : xs)
  | isDigit (element x) = Idx (rowIdx x) (colIdx x) (length number) : findNumberLocations rest
  | otherwise = findNumberLocations xs
  where
    (number, rest) = span (\y -> isDigit (element y) && rowIdx x == rowIdx y) (x : xs)

numberNeighbours :: RowSpan -> [TableIndex]
numberNeighbours (Idx i j len) = top ++ bottom ++ sides
  where
    top = map (i - 1,) [j - 1 .. j + len]
    bottom = map (i + 1,) [j - 1 .. j + len]
    sides = [(i, j - 1), (i, j + len)]

-- Algorithms
-------------
part1 :: [String] -> Maybe Int
part1 input = fmap sum $ mapM (fmap read . matrixSubrow input) $ filter testNumber $ findNumberLocations $ enumerateMatrix input
  where
    testNumber number = any (maybe False isValidSymbol . matrixElement input) (numberNeighbours number)
    isValidSymbol = (&&) <$> not . isDigit <*> (/= '.')

part2 :: [String] -> Maybe Int
part2 input = Just $ sum $ map handleAsterisk allAsterisks
  where
    inlineMatrix = enumerateMatrix input
    allAsterisks = filter ((== '*') . element) inlineMatrix
    findAdjacentNumbers (Idx i j _) = filter (elem (i, j) . numberNeighbours) (findNumberLocations inlineMatrix)
    handleAsterisk x =
      let adjacentNumbers = findAdjacentNumbers x
       in if length adjacentNumbers == 2
            then maybe 0 (product . map read) (mapM (matrixSubrow input) adjacentNumbers)
            else 0

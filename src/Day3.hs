{-# LANGUAGE TupleSections #-}

module Day3 where

import Data.Char (isDigit)

type RowIndex = Int

type ColIndex = Int

data IndexedElement a = Idx {rowIdx :: RowIndex, colIdx :: ColIndex, element :: a} deriving (Show)

listElement :: Int -> [a] -> Maybe a
listElement 0 (x : _) = Just x
listElement _ [] = Nothing
listElement i (_ : xs)
  | i < 0 = Nothing
  | otherwise = listElement (i - 1) xs

matrixElement :: RowIndex -> ColIndex -> [[a]] -> Maybe a
matrixElement i j arr = listElement i arr >>= \row -> listElement j row

matrixSubrow :: [[a]] -> IndexedElement Int -> Maybe [a]
matrixSubrow matrix (Idx i j len) = take len . drop j <$> listElement i matrix

enumerateMatrix :: [[a]] -> [IndexedElement a]
enumerateMatrix matrix = concat $ zipWith (map . uncurry . Idx) [0 ..] $ map (zip [0 ..]) matrix

findNumberLocations :: [IndexedElement Char] -> [IndexedElement Int]
findNumberLocations [] = []
findNumberLocations (x : xs)
  | isDigit (element x) = Idx (rowIdx x) (colIdx x) (length number) : findNumberLocations rest
  | otherwise = findNumberLocations xs
  where
    (number, rest) = span (\y -> isDigit (element y) && rowIdx x == rowIdx y) (x : xs)

scanLine :: (RowIndex -> ColIndex -> String -> Bool) -> RowIndex -> String -> [String]
scanLine testNumber rowIndex line = scanLineAcc [] (zip [0 ..] line)
  where
    scanLineAcc :: [String] -> [(Int, Char)] -> [String]
    scanLineAcc acc [] = acc
    scanLineAcc acc ((colIndex, x) : xs) =
      if isDigit x
        then
          let (number, rest) = span (isDigit . snd) ((colIndex, x) : xs)
           in scanLineAcc
                ( if testNumber rowIndex colIndex (map snd number)
                    then map snd number : acc
                    else acc
                )
                rest
        else scanLineAcc acc xs

surroundingIndices :: RowIndex -> ColIndex -> RowIndex -> ColIndex -> [(RowIndex, ColIndex)]
surroundingIndices minRow minCol maxRow maxCol = top ++ bottom ++ left ++ right
  where
    top = map (minRow - 1,) [minCol - 1 .. maxCol + 1]
    bottom = map (maxRow + 1,) [minCol - 1 .. maxCol + 1]
    left = map (,minCol - 1) [minRow .. maxRow]
    right = map (,maxCol + 1) [minRow .. maxRow]

part1 :: [String] -> Maybe Int
part1 input = Just $ sum $ concatMap (\(rowIndex, line) -> map read (scanLine testNumber rowIndex line)) (zip [0 ..] input)
  where
    testNumber rowIndex colIndex number = any (\(i, j) -> maybe False isValidSymbol (matrixElement i j input)) (numberNeighbours rowIndex colIndex number)
    numberNeighbours rowIndex colIndex number = surroundingIndices rowIndex colIndex rowIndex (colIndex + (length number - 1))
    isValidSymbol = (&&) <$> not . isDigit <*> (/= '.')

part2 :: [String] -> Maybe Int
part2 input = Just $ sum $ map handleAsterisk allAsterisks
  where
    inlineMatrix = enumerateMatrix input
    allAsterisks = filter ((== '*') . element) inlineMatrix
    numberLocations = findNumberLocations inlineMatrix
    numberSurroundingIndices (Idx rowIndex colIndex len) = surroundingIndices rowIndex colIndex rowIndex (colIndex + len - 1)
    findAdjacentNumbers (Idx i j _) = filter (elem (i, j) . numberSurroundingIndices) numberLocations
    handleAsterisk x =
      let adjacentNumbers = findAdjacentNumbers x
       in if length adjacentNumbers == 2
            then maybe 0 (product . map read) (mapM (matrixSubrow input) adjacentNumbers)
            else 0

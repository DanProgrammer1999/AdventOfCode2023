module Day1 (part1, part2) where

import Control.Applicative (Applicative (liftA2))
import Data.Char (intToDigit, isDigit)
import Data.List (find, isPrefixOf)

-- Part 1

part1 :: [String] -> Maybe String
part1 = fmap (show . sum) . mapM (fmap read . solveLine)
  where
    solveLine line = liftA2 (++) ((: []) <$> find isDigit line) ((: []) <$> find isDigit (reverse line))

-- Part 2

wordToDigit :: [(String, Int)]
wordToDigit =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

findAndReplaceDigitWords :: String -> String
findAndReplaceDigitWords [] = []
findAndReplaceDigitWords (x : xs) =
  case nextDigit of
    Just mapping -> intToDigit (snd mapping) : findAndReplaceDigitWords xs
    Nothing -> x : findAndReplaceDigitWords xs
  where
    nextDigit = find (\digitMapping -> fst digitMapping `isPrefixOf` (x : xs)) wordToDigit

part2 :: [String] -> Maybe String
part2 = part1 . map findAndReplaceDigitWords
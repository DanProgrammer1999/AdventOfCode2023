{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2) where

import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, breakOn, isSuffixOf, pack, splitOn, stripPrefix, takeWhile, unpack)
import Prelude hiding (takeWhile)

-- data
data Game = Game Int [Cubes] deriving (Show)

data Cubes = Cubes
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

maxDraw :: Cubes
maxDraw = Cubes {red = 12, green = 13, blue = 14}

-- Parse input

parseLine :: Text -> Maybe Game
parseLine line = fmap (`Game` games) gameId
  where
    (gameIdStr, gamesLine) = breakOn (pack ": ") line
    gameId = read . unpack <$> stripPrefix (pack "Game ") gameIdStr
    games = map parseCubes $ splitOn "; " (fromMaybe (pack "") (stripPrefix ": " gamesLine))

parseCubes :: Text -> Cubes
parseCubes cubes = Cubes {red = parseColour "red", green = parseColour "green", blue = parseColour "blue"}
  where
    parseColour colour = maybe 0 (read . unpack . takeWhile isDigit) (find (colour `isSuffixOf`) cubeStrings)
    cubeStrings = splitOn ", " cubes

-- Algorithms
runGames :: (Game -> a) -> ([a] -> b) -> [String] -> Maybe b
runGames runGame combineResults input = combineResults <$> mapM (fmap runGame . parseLine . pack) input

part1 :: [String] -> Maybe Int
part1 = runGames checkGame sum
  where
    checkGame (Game gameId games) = if all checkCubes games then gameId else 0
    checkCubes cubes = red cubes <= red maxDraw && green cubes <= green maxDraw && blue cubes <= blue maxDraw

part2 :: [String] -> Maybe Int
part2 = runGames calculateGamePower sum
  where
    calculateGamePower (Game _ games) = maximum (map red games) * maximum (map green games) * maximum (map blue games)
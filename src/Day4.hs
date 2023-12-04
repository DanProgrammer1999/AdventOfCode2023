{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day4 (part1, part2) where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as Map
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

data LotteryCard = LotteryCard {cardId :: Int, winningNumbers :: [Int], allNumbers :: [Int]} deriving (Show)

splitBy :: T.Text -> T.Text -> (T.Text, T.Text)
splitBy needle haystack =
  let (before, rest) = T.breakOn needle haystack
   in (before, fromMaybe T.empty (T.stripPrefix needle rest))

parseLine :: T.Text -> LotteryCard
parseLine line = LotteryCard cardId (parseNumbers winningNumsLine) (parseNumbers ourNumsLine)
  where
    cardId = read $ T.unpack $ T.filter isDigit label
    parseNumbers = map read . filter (/= "") . map (T.unpack . T.strip) . T.split (== ' ') . T.strip
    (label, numbers) = splitBy ": " line
    (winningNumsLine, ourNumsLine) = splitBy " | " numbers

cardValue :: LotteryCard -> Int
cardValue = length . liftA2 intersect allNumbers winningNumbers

part1 :: [String] -> Maybe Int
part1 = Just . sum . map ((2 ^) . subtract 1) . filter (> 0) . map (cardValue . parseLine . T.pack)

calculateBonusCards :: LotteryCard -> Map.IntMap Int -> Map.IntMap Int
calculateBonusCards currentCard cardSet = foldl (flip (Map.adjust (+ cardCount))) cardSet bonusCardIds
  where
    currId = cardId currentCard
    cardCount = Map.findWithDefault 0 (cardId currentCard) cardSet
    bonusCardIds = [currId + 1 .. currId + cardValue currentCard]

part2 :: [String] -> Maybe Int
part2 input = Just $ countCards initialCardSet cards
  where
    countCards cardSet [] = sum cardSet
    countCards cardSet (c : cs) = countCards (calculateBonusCards c cardSet) cs

    initialCardSet = Map.fromAscList $ map ((,1) . cardId) cards
    cards = map (parseLine . T.pack) input
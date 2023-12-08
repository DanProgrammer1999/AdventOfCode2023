{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import Data.Char (isSpace)
import Data.List (group, sort, sortBy)
import Utils (splitOn)

-- Types
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Joker | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data CardCombination = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

class (Eq a, Ord a) => Card a where
  cardCombination :: [a] -> CardCombination

newtype Hand cardType = Hand {getHand :: [cardType]} deriving (Show, Eq)

instance (Card a) => Ord (Hand a) where
  compare (Hand hand1) (Hand hand2)
    | combination1 /= combination2 = compare combination1 combination2
    | otherwise = foldl1 (\a b -> if a /= EQ then a else b) (zipWith compare hand1 hand2)
    where
      combination1 = cardCombination hand1
      combination2 = cardCombination hand2

-- Part1 Instance
newtype Card' = Card' {getCard' :: CardValue} deriving (Show, Eq)

instance Card Card' where
  cardCombination = frequenciesToCombinations . cardFrequencies

instance Ord Card'' where
  compare (Card'' c1) (Card'' c2)
    | c1 == c2 = EQ
    | c1 == Joker = LT -- Joker is less than all other cards
    | c2 == Joker = GT -- any card is more than Joker
    | otherwise = compare c1 c2

-- Part2 Instance
newtype Card'' = Card'' {getCard'' :: CardValue} deriving (Show, Eq)

instance Ord Card' where
  compare (Card' c1) (Card' c2) = compare c1 c2

instance Card Card'' where
  cardCombination :: [Card''] -> CardCombination
  cardCombination cards = case frequenciesWithoutJ of
    [] -> FiveOfAKind -- all jokers
    (highest : rest) -> frequenciesToCombinations ((highest + jokerCount) : rest)
    where
      frequenciesWithoutJ = cardFrequencies $ filter (/= Card'' Joker) cards
      jokerCount = length $ filter (== Card'' Joker) cards

-- Card combinations
cardFrequencies :: (Card a) => [a] -> [Int]
cardFrequencies = sortBy (flip compare) . map length . group . sortBy (flip compare)

frequenciesToCombinations :: [Int] -> CardCombination
frequenciesToCombinations [5] = FiveOfAKind
frequenciesToCombinations [4, 1] = FourOfAKind
frequenciesToCombinations [3, 2] = FullHouse
frequenciesToCombinations [3, 1, 1] = ThreeOfAKind
frequenciesToCombinations [2, 2, 1] = TwoPair
frequenciesToCombinations [2, 1, 1, 1] = OnePair
frequenciesToCombinations [1, 1, 1, 1, 1] = HighCard
frequenciesToCombinations _ = error "Not 5 cards 🤨"

-- Parsing
parseLine :: (CardValue -> a) -> String -> Maybe (Hand a, Int)
parseLine makeCard line = case splitOn isSpace line of
  [hand, bid] -> fmap ((,read bid) . Hand) (mapM (fmap makeCard . (`lookup` cardSymbols)) hand)
  _ -> Nothing
  where
    cardSymbols = zip "23456789TJQKA" (map toEnum [0 ..])

-- Solutions
algorithm :: (Card a) => (CardValue -> a) -> [String] -> Maybe Int
algorithm makeCard input = sum . zipWith (*) [1 ..] . map snd . sort <$> mapM (parseLine makeCard) input

part1 :: [String] -> Maybe Int
part1 = algorithm Card'

part2 :: [String] -> Maybe Int
part2 = algorithm Card''
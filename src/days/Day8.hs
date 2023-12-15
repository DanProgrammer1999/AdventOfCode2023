{-# LANGUAGE LambdaCase #-}

module Day8 where

import Data.Char (isSpace, isUpper)
import Data.List (find, isSuffixOf)
import Data.Maybe (fromJust, listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, between, char, choice, count, eof, many, readP_to_S, satisfy, sepBy1, skipSpaces, string)
import Prelude hiding (Left, Right)

data Direction = Left | Right deriving (Show, Eq)

data Node = Node {value :: String, leftNode :: String, rightNode :: String} deriving (Show, Eq)

-- Parsing
parseDirections :: ReadP [Direction]
parseDirections = many $ choice [char 'L' >> return Left, char 'R' >> return Right]

parseNode :: ReadP Node
parseNode = do
  let parseNodeValue = count 3 $ satisfy isUpper
  v <- parseNodeValue
  _ <- string " = "
  [left, right] <- between (char '(') (char ')') (sepBy1 parseNodeValue (string ", "))
  return $ Node v left right

parseInput :: ReadP ([Node], [Direction])
parseInput = do
  directions <- parseDirections
  _ <- skipSpaces
  nodes <- sepBy1 parseNode (satisfy isSpace)
  _ <- eof
  return (nodes, directions)

runParseInput :: String -> Maybe ([Node], [Direction])
runParseInput = fmap fst . listToMaybe . readP_to_S parseInput

-- Solutions
makeStep :: [Node] -> Node -> Direction -> Node
makeStep nodes currNode = \case
  Left -> fromJust $ find ((== leftNode currNode) . value) nodes
  Right -> fromJust $ find ((== rightNode currNode) . value) nodes

stepsToFinish :: [Node] -> [Direction] -> Node -> (String -> Bool) -> Int
stepsToFinish nodes directions startNode isFinishValue = stepsToFinishRec directions startNode 0
  where
    stepsToFinishRec [] currNode steps = stepsToFinishRec directions currNode steps
    stepsToFinishRec (dir : dirs) currNode steps =
      if isFinishValue (value currNode)
        then steps
        else stepsToFinishRec dirs (makeStep nodes currNode dir) (steps + 1)

part1 :: String -> Maybe Int
part1 input = do
  (nodes, directions) <- runParseInput input
  startingNode <- find ((== "AAA") . value) nodes
  return $ stepsToFinish nodes directions startingNode (== "ZZZ")

part2 :: String -> Maybe Int
part2 input = uncurry solution2 <$> runParseInput input

solution2 :: [Node] -> [Direction] -> Int
solution2 nodes directions = foldl1 lcm $ map (\start -> stepsToFinish nodes directions start ("Z" `isSuffixOf`)) startingNodes
  where
    startingNodes = filter (("A" `isSuffixOf`) . value) nodes
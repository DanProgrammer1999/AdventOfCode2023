{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.List (find)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe, mapMaybe)

data Direction = North | South | East | West deriving (Show, Eq)

data NodeType
  = Vertical
  | Horizontal
  | Bend Direction Direction
  | Ground
  | Start
  deriving (Show, Eq)

readNodeType :: Char -> NodeType
readNodeType = \case
  '|' -> Vertical
  '-' -> Horizontal
  'L' -> Bend North East
  'J' -> Bend North West
  '7' -> Bend South East
  'F' -> Bend South West
  '.' -> Ground
  'S' -> Start
  d -> error $ "unknown direction " ++ show d

type Position = (Int, Int)

type NodeMap = M.Map Position NodeType

data Node = Node {nodePosition :: Position, nodeType :: NodeType} deriving (Show, Eq)

readInput :: String -> (Node, NodeMap)
readInput =
  returnResult
    . concat
    . zipWith (\i row -> map (\(j, node) -> ((i, j), node)) row) [0 ..]
    . map (zip [0 ..] . map readNodeType)
    . lines
  where
    returnResult list = let nodeMap = M.fromAscList list in (makeNode nodeMap (startingNode list), nodeMap)
    startingNode list = fst . fromJust $ find ((== Start) . snd) list

findPath :: NodeMap -> Node -> [[Node]]
findPath nodeMap startingNode = findPathRec [] startingNode
  where
    findPathRec visited current = map (fromMaybe visited . listToMaybe . findPathRec (current : visited)) (filter (`notElem` visited) (neighbours current))

    neighbours = mapMaybe (tryMakeNode nodeMap) . (nodeNeighbours nodeMap)

nodeNeighbours :: NodeMap -> Node -> [Position]
nodeNeighbours nodeMap (Node (i, j) nodeType') = case nodeType' of
  Vertical -> [(i - 1, j), (i + 1, j)]
  Horizontal -> [(i, j - 1), (i, j + 1)]
  Bend dirA dirB -> [travelDirection (i, j) dirA, travelDirection (i, j) dirB]
  Ground -> []
  Start -> [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

travelDirection :: Position -> Direction -> Position
travelDirection (i, j) = \case
  North -> (i - 1, j)
  South -> (i + 1, j)
  East -> (i, j - 1)
  West -> (i, j + 1)

tryMakeNode :: NodeMap -> Position -> Maybe Node
tryMakeNode nodeMap position = fmap (Node position) (nodeMap !? position)

makeNode :: NodeMap -> Position -> Node
makeNode nodeMap position = Node position (nodeMap ! position)

-- part1 :: String -> [[Node]]
part1 :: [Char] -> [[Node]]
part1 input = undefined

-- .....
-- .F-7.
-- .|.|.
-- .L-J.
-- .....
-- (1, 1) Start, (1, 2) Horizontal, (1, 3) SouthWest, (2, 1) Vertical, (2, 3) Vertical, (3, 1) NorthEast, (3, 2) Horizontal, (3, 3) NorthWest
-- [(1, 2), (2, 1)] -> []
-- Visited: {(1, 1)}
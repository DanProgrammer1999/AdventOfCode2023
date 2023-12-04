module Main (main) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import System.TimeIt

type Algorithm a = [String] -> a

main :: IO ()
main = do
  putStrLn $ "🎄 Welcome to AoC 2023 🎄\n" ++ lineSeparator ++ "\n"
  mapM_ (uncurry runDay) days

days :: [(Int, [Algorithm (Maybe Int)])]
days =
  [ (1, [Day1.part1, Day1.part2]),
    (2, [Day2.part1, Day2.part2]),
    (3, [Day3.part1, Day3.part2]),
    (4, [Day4.part1, Day4.part2])
  ]

getFilePath :: Int -> FilePath
getFilePath day = "inputs/day" ++ show day ++ ".txt"

runDay :: (Show a) => Int -> [Algorithm a] -> IO ()
runDay dayNumber parts = do
  putStrLn ("Day " ++ show dayNumber)
  mconcat $ zipWith runPart [1 :: Int ..] parts
  putStrLn $ lineSeparator ++ "\n"
  where
    runPart part = runAlgorithm ("Part " ++ show part) (getFilePath dayNumber)

runAlgorithm :: (Show a) => String -> FilePath -> Algorithm a -> IO ()
runAlgorithm description filePath algorithm = do
  input <- readFile filePath
  let result = algorithm (lines input)

  timeIt $ putStrLn $ "Running " ++ description ++ ". Result: " ++ show (algorithm (lines input))

lineSeparator :: String
lineSeparator = "--------------------"

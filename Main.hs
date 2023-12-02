module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Day1

type Algorithm = [String] -> Maybe String

main :: IO ()
main = do
  putStrLn $ "🎄 Welcome to AoC 2023 🎄\n" ++ lineSeparator ++ "\n"
  mapM_ (uncurry runDay) days

days :: [(Int, [Algorithm])]
days =
  [ (1, [Day1.part1, Day1.part2])
  ]

getFilePath :: Int -> FilePath
getFilePath day = "inputs/day" ++ show day ++ ".txt"

runDay :: Int -> [Algorithm] -> IO ()
runDay dayNumber parts = do
  putStrLn ("Day " ++ show dayNumber)
  mconcat $ zipWith runPart [1 ..] parts
  putStrLn $ lineSeparator ++ "\n"
  where
    runPart :: Int -> Algorithm -> IO ()
    runPart part = runAlgorithm ("Part " ++ show part) (getFilePath dayNumber)

runAlgorithm :: String -> FilePath -> Algorithm -> IO ()
runAlgorithm description filePath algorithm = do
  input <- readFile filePath
  let result = fromMaybe "!! ERROR !!" $ algorithm (lines input)

  putStrLn $ "Running " ++ description ++ ". Result: " ++ result

lineSeparator :: String
lineSeparator = "--------------------"

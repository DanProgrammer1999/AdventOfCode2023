module Main (main) where

import qualified Day5
import System.TimeIt

type Algorithm a = [String] -> a

main :: IO ()
main = do
  runAlgorithm 5 show Day5.part1
  runAlgorithm 5 show Day5.part2 -- takes ~12 mins (752.81sec)

getFilePath :: Int -> FilePath
getFilePath day = "inputs/day" ++ show day ++ ".txt"

getSampleFilePath :: Int -> FilePath
getSampleFilePath day = "inputs/day" ++ show day ++ "_sample.txt"

runSample :: Int -> (a -> String) -> Algorithm a -> IO ()
runSample day renderOutput algorithm = putStrLn ("Day " ++ show day ++ " Sample. Result:\n") >> runWithFile (getSampleFilePath day) renderOutput algorithm

runAlgorithm :: Int -> (a -> String) -> Algorithm a -> IO ()
runAlgorithm day renderOutput algorithm = putStrLn ("Day " ++ show day ++ ". Result:\n") >> runWithFile (getFilePath day) renderOutput algorithm

runWithFile :: FilePath -> (a -> String) -> Algorithm a -> IO ()
runWithFile filePath renderOutput algorithm = do
  input <- readFile filePath
  timeIt $ putStrLn $ renderOutput $ algorithm $ lines input
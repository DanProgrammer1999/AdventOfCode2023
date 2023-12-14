module Main (main) where

import Data.List (intercalate)
import qualified Day11
import System.TimeIt

type Algorithm a = String -> a

main :: IO ()
main = do
  runAlgorithm 11 show Day11.part1

showList' :: (Show a) => [a] -> String
showList' = intercalate "\n" . map show

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
  timeIt $ putStrLn $ renderOutput $ algorithm input
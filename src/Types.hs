module Types where

data Options = Options
  { dayNumber :: Int,
    isSample :: Bool,
    parts :: (Bool, Bool)
  }
  deriving (Show)

type Algorithm = String -> String
{-# LANGUAGE ApplicativeDo #-}

module ArgParser (programParser, programHeader) where

import Options.Applicative
import Types

programParser :: ParserInfo Options
programParser = info (helper <*> optionsParser) programInfo

programHeader :: String
programHeader = "🎄 AoC 2023 — Haskell λ 🎄"

programInfo :: InfoMod a
programInfo =
  fullDesc
    <> header "Run algorithms for a given DAY problem of AoC 2023"
    <> progDesc programHeader

optionsParser :: Parser Options
optionsParser = Options <$> dayNumberParser <*> isSampleParser <*> partsParser

dayNumberParser :: Parser Int
dayNumberParser = option auto (long "day" <> short 'd' <> metavar "DAY_NUMBER" <> help "Which day to run")

isSampleParser :: Parser Bool
isSampleParser = switch (long "sample" <> help "Run with sample input")

partsParser :: Parser (Bool, Bool)
partsParser = chooseParts <$> part1 <*> part2
  where
    part1 = switch (long "part1" <> help "Run part 1")
    part2 = switch (long "part2" <> help "Run part 2")
    chooseParts p1 p2 = if p1 == p2 then (True, True) else (p1, p2)

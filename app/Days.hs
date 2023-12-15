{-# LANGUAGE TemplateHaskell #-}

module Days where

import Control.Monad
import Language.Haskell.TH

days :: [(Algorithm, Algorithm)]
days = [
  (Day1.part1, Day1.part2),
  (Day1.part1, Day1.part2),
]

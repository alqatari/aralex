module Util.RootMatching where

import Prelude
import Data.String.CodeUnits (toCharArray)
import Data.Array (all)
import Data.Foldable (any)
import Util.Arabic (removeTashkeel)

-- | Check if a word contains all letters from a root (order doesn't matter)
wordContainsRoot :: String -> String -> Boolean
wordContainsRoot wordText rootText =
  let word = removeTashkeel wordText
      root = removeTashkeel rootText
      rootLetters = toCharArray root
      wordLetters = toCharArray word
  in all (\rootLetter -> any (\wordLetter -> wordLetter == rootLetter) wordLetters) rootLetters

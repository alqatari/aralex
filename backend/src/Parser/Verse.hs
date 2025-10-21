{-|
Module      : Parser.Verse
Description : Parser for Quranic verse text
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Parses pipe-delimited verse text file.
Format: surah|verse|text
-}

module Parser.Verse
  ( -- * Parsing
    parseVerseLine
  , parseVerseFile
  , parseAllVerses
    -- * Loading
  , loadVerseText
    -- * Utilities
  , countWords
  ) where

import Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Domain.Verse

-- | Parse single verse line: "surah|verse|text"
parseVerseLine :: Parser VerseText
parseVerseLine = do
  s <- decimal
  _ <- char '|'
  v <- decimal
  _ <- char '|'
  txt <- A.takeText
  case mkVerseRef s v of
    Just ref -> do
      let wc = countWords txt
      pure $ VerseText ref txt wc
    Nothing -> fail $ "Invalid verse reference: " <> show s <> ":" <> show v

-- | Count words in Arabic text
countWords :: Text -> Int
countWords = length . T.words

-- | Parse all verses from file content
-- Skip blank lines and comment lines (starting with #)
parseAllVerses :: Text -> Either String [VerseText]
parseAllVerses content =
  let validLines = filter isValidLine (T.lines content)
  in mapM (parseOnly parseVerseLine) validLines
  where
    isValidLine line =
      not (T.null line) &&
      not (T.isPrefixOf "#" line)

-- | Parse verse text file
parseVerseFile :: FilePath -> IO (Either String [VerseText])
parseVerseFile path = do
  content <- TIO.readFile path
  pure $ parseAllVerses content

-- | Load verse text from data directory
loadVerseText :: FilePath -> IO (Either String [VerseText])
loadVerseText dataDir = do
  let versePath = dataDir ++ "/quran_text/quran-simple.txt"
  parseVerseFile versePath

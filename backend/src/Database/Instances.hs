{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Database.Instances
Description : SQLite FromRow instances for domain types
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

FromRow instances for converting database rows to domain types.
This module avoids orphan instance warnings by keeping instances close to DB operations.
-}

module Database.Instances () where

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Domain.Dictionary (DictEntry(..), RootText(..))
import Domain.Verse (VerseText(..), mkVerseRef)

-- | FromRow instance for DictEntry
-- Maps database columns: id, source_id, root, root_normalized, definition_arabic, dictionary_source
instance FromRow DictEntry where
  fromRow = do
    _id <- field :: RowParser Int
    _sourceId <- field :: RowParser Int
    rootText <- field :: RowParser Text
    rootNorm <- field :: RowParser Text
    defAr <- field :: RowParser Text
    dictSrc <- field :: RowParser Text
    pure $ DictEntry
      { root = RootText rootText
      , rootNormalized = RootText rootNorm
      , definitionArabic = defAr
      , dictionarySource = dictSrc
      }

-- | FromRow instance for VerseText
-- Maps database columns: id, surah, verse, text, word_count
instance FromRow VerseText where
  fromRow = do
    _id <- field :: RowParser Int
    surahNum <- field :: RowParser Int
    verseNum <- field :: RowParser Int
    verseText <- field :: RowParser Text
    wc <- field :: RowParser Int
    -- Database should contain valid verse references
    -- If invalid, this represents a data integrity issue
    case mkVerseRef surahNum verseNum of
      Just ref -> pure $ VerseText ref verseText wc
      Nothing -> error $ "Invalid verse reference in database: " <> show surahNum <> ":" <> show verseNum

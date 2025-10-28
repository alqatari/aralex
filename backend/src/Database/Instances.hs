{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Database.Instances
-- Description : SQLite FromRow instances for domain types
-- Copyright   : (c) Ali Al-Qatari, 2025
-- License     : MIT
--
-- FromRow instances for converting database rows to domain types.
-- This module avoids orphan instance warnings by keeping instances close to DB operations.
module Database.Instances () where

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Domain.Dictionary (DictEntry (..), RootText (..))
import Domain.Irab (IrabDetail (..), IrabSource (..))
import Domain.Verse (VerseText (..), mkVerseRef)

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
    pure $
      DictEntry
        { root = RootText rootText,
          rootNormalized = RootText rootNorm,
          definitionArabic = defAr,
          dictionarySource = dictSrc
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

-- | FromRow instance for IrabDetail
-- Maps database columns: word_position, word_surface, grammatical_role, construction_type,
-- case_type, case_marker, case_position, full_irab_text, source, confidence, volume_number, page_number
instance FromRow IrabDetail where
  fromRow = do
    pos <- field :: RowParser Int
    surface <- field :: RowParser Text
    role <- field :: RowParser (Maybe Text)
    construction <- field :: RowParser (Maybe Text)
    caseType <- field :: RowParser Text
    marker <- field :: RowParser (Maybe Text)
    position <- field :: RowParser (Maybe Text)
    fullText <- field :: RowParser Text
    sourceStr <- field :: RowParser Text
    conf <- field :: RowParser Double
    vol <- field :: RowParser (Maybe Int)
    page <- field :: RowParser (Maybe Int)

    let source = case sourceStr of
          "inferred" -> Inferred
          "aljadwal_ocr" -> AljadwalOCR
          "manual" -> Manual
          _ -> Inferred

    pure $
      IrabDetail
        { irabWordPosition = pos,
          irabWordSurface = surface,
          irabGrammaticalRole = role,
          irabConstructionType = construction,
          irabCaseType = caseType,
          irabCaseMarker = marker,
          irabCasePosition = position,
          irabFullText = fullText,
          irabSource = source,
          irabConfidence = conf,
          irabVolumeNumber = vol,
          irabPageNumber = page
        }

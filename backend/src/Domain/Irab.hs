{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Domain.Irab
-- Description : Traditional Arabic grammatical analysis (إعراب) domain types
-- Copyright   : (c) Ali Al-Qatari, 2025
-- License     : MIT
--
-- Domain types for traditional Arabic grammatical analysis (i'rab).
module Domain.Irab
  ( IrabDetail (..),
    IrabSource (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Source of i'rab data
data IrabSource
  = -- | Rule-based inference from morphology
    Inferred
  | -- | Extracted from الجدول في إعراب القرآن via OCR
    AljadwalOCR
  | -- | Manually corrected/validated
    Manual
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Traditional grammatical analysis for a single word
data IrabDetail = IrabDetail
  { -- | Position in verse (1-indexed)
    irabWordPosition :: !Int,
    -- | Word surface form
    irabWordSurface :: !Text,
    -- | مبتدأ، خبر، فاعل، etc.
    irabGrammaticalRole :: !(Maybe Text),
    -- | اسم إشارة، فعل ماضي، etc.
    irabConstructionType :: !(Maybe Text),
    -- | مبني or معرب
    irabCaseType :: !Text,
    -- | على السكون، بالضمة، etc.
    irabCaseMarker :: !(Maybe Text),
    -- | في محل رفع، في محل نصب، etc.
    irabCasePosition :: !(Maybe Text),
    -- | Complete traditional i'rab text
    irabFullText :: !Text,
    -- | Data source
    irabSource :: !IrabSource,
    -- | Confidence score (0.0-1.0)
    irabConfidence :: !Double,
    -- | Volume number (for الجدول source)
    irabVolumeNumber :: !(Maybe Int),
    -- | Page number (for الجدول source)
    irabPageNumber :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

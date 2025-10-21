{-|
Module      : Domain.Verse
Description : Domain types for Quranic verses and references
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Types for Quranic verse references with validation.
-}

module Domain.Verse
  ( -- * Verse References
    VerseRef(..)
  , mkVerseRef
  , verseKey
    -- * Re-exports from Data.QuranMetadata
  , module Data.QuranMetadata
    -- * Verse Text
  , VerseText(..)
  , VerseWithRoot(..)
    -- * Utilities
  , isValidSurah
  , isValidVerse
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.QuranMetadata

-- | Validated verse reference
data VerseRef = VerseRef
  { refSurah :: !Int  -- ^ Surah number (1-114)
  , refVerse :: !Int  -- ^ Verse number (1-max for surah)
  } deriving (Eq, Ord, Show, Generic)

-- Use default Generic JSON encoding to match PureScript bridge
deriving instance ToJSON VerseRef
deriving instance FromJSON VerseRef

-- | Smart constructor with validation
mkVerseRef :: Int -> Int -> Maybe VerseRef
mkVerseRef s v
  | not (isValidSurah s) = Nothing
  | not (isValidVerse s v) = Nothing
  | otherwise = Just (VerseRef s v)

-- | Format verse reference as "surah:verse"
verseKey :: VerseRef -> Text
verseKey (VerseRef s v) = T.pack (show s) <> ":" <> T.pack (show v)

-- Note: SurahInfo, RevelationType, and related functions are imported from Data.QuranMetadata

-- | Check if surah number is valid (1-114)
isValidSurah :: Int -> Bool
isValidSurah s = s >= 1 && s <= 114

-- | Check if verse number is valid for given surah
isValidVerse :: Int -> Int -> Bool
isValidVerse s v = isValidVerseNumber s v

-- | Verse with its text
data VerseText = VerseText
  { verseRef  :: VerseRef
  , text      :: Text      -- ^ Uthmani script
  , wordCount :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON VerseText where
  toJSON VerseText{..} = A.object
    [ "surah"      .= refSurah verseRef
    , "verse"      .= refVerse verseRef
    , "key"        .= verseKey verseRef
    , "text"       .= text
    , "word_count" .= wordCount
    ]

instance FromJSON VerseText where
  parseJSON = A.withObject "VerseText" $ \o -> do
    s <- o .: "surah"
    v <- o .: "verse"
    t <- o .: "text"
    wc <- o .: "word_count"
    case mkVerseRef s v of
      Just ref -> pure $ VerseText ref t wc
      Nothing  -> fail "Invalid verse reference"

-- | Verse with information about which words contain a specific root
data VerseWithRoot = VerseWithRoot
  { vwrVerseRef     :: VerseRef
  , vwrText         :: Text      -- ^ Full verse text
  , vwrRoot         :: Text      -- ^ The root being searched
  , vwrSurahName    :: Text      -- ^ Arabic name of the surah
  , vwrWordIndices  :: [Int]     -- ^ 1-based indices of words containing the root
  , vwrOccurrences  :: !Int      -- ^ Number of times root appears in this verse
  } deriving (Eq, Show, Generic)

-- Use default Generic JSON encoding to match PureScript bridge
deriving instance ToJSON VerseWithRoot
deriving instance FromJSON VerseWithRoot

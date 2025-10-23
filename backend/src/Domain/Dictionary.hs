{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Domain.Dictionary
Description : Domain types for classical Arabic dictionary entries
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Types representing entries from 6 classical Arabic dictionaries (Qamus excluded).
-}

module Domain.Dictionary
  ( -- * Dictionary Sources
    DictionaryId(..)
  , DictionarySource(..)
  , dictionaryMetadata
    -- * Dictionary Entries
  , DictEntry(..)
  , RootText(..)
  , normalizeRoot
  , normalizeArabicText
    -- * Utilities
  , getDictSourceId
  , isEarlyDictionary
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Unique identifiers for the 6 classical dictionaries (Qamus excluded for size/cost)
data DictionaryId
  = AinId        -- ^ Kitab al-Ain (786 CE) - 2,707 entries
  | SihahId      -- ^ Al-Sihah (1003 CE) - 5,594 entries
  | MaqayisId    -- ^ Maqayis al-Lugha (1004 CE) - 4,794 entries
  | MuhkamId     -- ^ Al-Muhkam (1066 CE) - 6,584 entries
  | MufradatId   -- ^ Al-Mufradat (1108 CE) - 1,602 entries
  | LisanId      -- ^ Lisan al-Arab (1311 CE) - 9,029 entries
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON DictionaryId
instance FromJSON DictionaryId

-- | Metadata about a dictionary source
data DictionarySource = DictionarySource
  { dictId      :: DictionaryId
  , author      :: Text
  , title       :: Text
  , deathYear   :: Int  -- ^ Year CE
  , entryCount  :: Int
  , filename    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON DictionarySource
instance FromJSON DictionarySource

-- | Metadata for all 6 dictionaries (deduplicated counts, Qamus excluded)
dictionaryMetadata :: [DictionarySource]
dictionaryMetadata =
  [ DictionarySource
      { dictId = AinId
      , author = "Al-Khalīl b. Aḥmad al-Farāhīdī"
      , title = "Kitāb al-ʿAin"
      , deathYear = 786
      , entryCount = 2707
      , filename = "ain_entries_20251017_002737.json"
      }
  , DictionarySource
      { dictId = SihahId
      , author = "Al-Jawharī"
      , title = "Al-Ṣiḥāḥ"
      , deathYear = 1003
      , entryCount = 5594
      , filename = "sihah_entries_20251017_092639.json"
      }
  , DictionarySource
      { dictId = MaqayisId
      , author = "Ibn Fāris"
      , title = "Maqāyīs al-Lugha"
      , deathYear = 1004
      , entryCount = 4794
      , filename = "maqayis_entries_20251017_021111.json"
      }
  , DictionarySource
      { dictId = MuhkamId
      , author = "Ibn Sīda"
      , title = "Al-Muḥkam"
      , deathYear = 1066
      , entryCount = 6584
      , filename = "al_muhkam_entries_20251017_111834.json"
      }
  , DictionarySource
      { dictId = MufradatId
      , author = "Al-Rāghib al-Iṣfahānī"
      , title = "Al-Mufradāt"
      , deathYear = 1108
      , entryCount = 1602
      , filename = "mofradat_merged_20251023_200836.json"
      }
  , DictionarySource
      { dictId = LisanId
      , author = "Ibn Manẓūr"
      , title = "Lisān al-ʿArab"
      , deathYear = 1311
      , entryCount = 9029
      , filename = "lisan_entries_20251017_181614.json"
      }
  ]

-- | Arabic root with optional diacritics
newtype RootText = RootText { unRoot :: Text }
  deriving (Eq, Ord, Show, Generic)

-- ToJSON: Use Generic derivation to match PureScript structure
instance ToJSON RootText

-- FromJSON: Parse from plain string (for backward compatibility with source files)
instance FromJSON RootText where
  parseJSON = A.withText "RootText" (pure . RootText)

-- | Remove diacritics from root for normalization
normalizeRoot :: RootText -> RootText
normalizeRoot (RootText txt) = RootText $ T.filter (not . isDiacritic) txt
  where
    isDiacritic c = c `elem` ['\x064B'..'\x0652'] -- Arabic diacritics range

-- | Comprehensive Arabic text normalization (matches database normalization)
-- Removes diacritics and normalizes hamza, alif, and ya variations
normalizeArabicText :: Text -> Text
normalizeArabicText = normalizeYa . normalizeAlif . normalizeHamza . removeDiacritics
  where
    -- Remove Arabic diacritics (U+064B to U+0652)
    removeDiacritics = T.filter (not . isDiacritic)
    isDiacritic c = c `elem` ['\x064B'..'\x0652']

    -- Normalize hamza variations
    normalizeHamza txt = T.map (\c -> case c of
      '\x0623' -> '\x0627'  -- أ → ا (alif with hamza above)
      '\x0625' -> '\x0627'  -- إ → ا (alif with hamza below)
      '\x0622' -> '\x0627'  -- آ → ا (alif with madda)
      '\x0624' -> '\x0648'  -- ؤ → و (waw with hamza)
      '\x0626' -> '\x064A'  -- ئ → ي (ya with hamza)
      _ -> c
      ) txt

    -- Normalize alif variations
    normalizeAlif txt =
      let mapped = T.map (\c -> case c of
            '\x0671' -> '\x0627'  -- ٱ → ا (alif wasla)
            '\x0649' -> '\x064A'  -- ى → ي (alif maqsura)
            _ -> c
            ) txt
      in T.filter (/= '\x0670') mapped  -- Remove alif khanjariyah

    -- Normalize ya variations
    normalizeYa txt = T.map (\c -> case c of
      '\x06CC' -> '\x064A'  -- ی → ي (Farsi ya)
      _ -> c
      ) txt

-- | Dictionary entry (lightweight - only essential fields)
data DictEntry = DictEntry
  { root              :: RootText  -- ^ Arabic root
  , rootNormalized    :: RootText  -- ^ Normalized root (for search)
  , definitionArabic  :: Text      -- ^ Full Arabic definition
  , dictionarySource  :: Text      -- ^ Dictionary source (e.g., "Al-Khalīl b. Aḥmad al-Farāhīdī, Kitāb al-ʿAin (d. 786 CE)")
  } deriving (Eq, Show, Generic)

-- ToJSON: Use camelCase for API responses (matches PureScript field names)
instance ToJSON DictEntry

-- FromJSON: Parse from snake_case (matches dictionary JSON files)
instance FromJSON DictEntry where
  parseJSON = A.withObject "DictEntry" $ \o -> do
    root              <- o .:  "root"
    rootNormalized    <- o .:  "root_normalized"
    definitionArabic  <- o .:  "definition_arabic"
    dictionarySource  <- o .:  "dictionary_source"
    -- Ignore: url, permalink, scraped_timestamp
    pure DictEntry{..}

-- | Extract dictionary ID from source string
getDictSourceId :: Text -> Maybe DictionaryId
getDictSourceId src
  | "Farāhīdī" `T.isInfixOf` src = Just AinId
  | "Jawharī"  `T.isInfixOf` src = Just SihahId
  | "Fāris"    `T.isInfixOf` src = Just MaqayisId
  | "Sīda"     `T.isInfixOf` src = Just MuhkamId
  | "Rāghib"   `T.isInfixOf` src = Just MufradatId
  | "Manẓūr"   `T.isInfixOf` src = Just LisanId
  | otherwise = Nothing  -- Qamus excluded

-- | Check if dictionary is from early period (pre-1100 CE)
isEarlyDictionary :: DictionaryId -> Bool
isEarlyDictionary dictId = case dictId of
  AinId     -> True
  SihahId   -> True
  MaqayisId -> True
  MuhkamId  -> True
  MufradatId -> False
  LisanId   -> False

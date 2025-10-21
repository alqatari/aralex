{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Parser.QuranMetadata
Description : XML parser for complete Quran metadata from quran-data.xml
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Parses Tanzil.net quran-data.xml to extract 7 organizational systems:
1. Suras (114 chapters) - already in Data.QuranMetadata
2. Juz (30 parts)
3. Quarters/Hizbs (240 quarters)
4. Manzils (7 stations)
5. Rukus (556 sections) - MOST IMPORTANT for thematic analysis
6. Pages (604 pages)
7. Sajdas (15 prostrations)
-}

module Parser.QuranMetadata
  ( -- * Types
    Juz(..)
  , Quarter(..)
  , Manzil(..)
  , Ruku(..)
  , Page(..)
  , Sajda(..)
  , SajdaType(..)
  , QuranMetadataFull(..)
    -- * Parsers
  , parseQuranMetadataXML
  , parseQuranMetadataFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.XML.Light
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Juz (30 parts) - Equal divisions for monthly reading
data Juz = Juz
  { juzIndex :: !Int
  , juzStartSurah :: !Int
  , juzStartAya :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON Juz
instance FromJSON Juz

-- | Quarter (240 quarters) - Finer divisions (Hizb quarters)
data Quarter = Quarter
  { quarterIndex :: !Int
  , quarterStartSurah :: !Int
  , quarterStartAya :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON Quarter
instance FromJSON Quarter

-- | Manzil (7 stations) - Weekly reading plan
data Manzil = Manzil
  { manzilIndex :: !Int
  , manzilStartSurah :: !Int
  , manzilStartAya :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON Manzil
instance FromJSON Manzil

-- | Ruku (556 sections) - Thematic/logical verse groupings
-- MOST IMPORTANT for phonosemantic analysis
data Ruku = Ruku
  { rukuIndex :: !Int
  , rukuSurah :: !Int
  , rukuStartAya :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON Ruku
instance FromJSON Ruku

-- | Page (604 pages) - Madina Mushaf pagination
data Page = Page
  { pageIndex :: !Int
  , pageStartSurah :: !Int
  , pageStartAya :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON Page
instance FromJSON Page

-- | Sajda type (prostration)
data SajdaType = Obligatory | Recommended
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON SajdaType
instance FromJSON SajdaType

-- | Sajda (15 prostrations) - Prostration verses
data Sajda = Sajda
  { sajdaIndex :: !Int
  , sajdaSurah :: !Int
  , sajdaAya :: !Int
  , sajdaType :: !SajdaType
  } deriving (Eq, Show, Generic)

instance ToJSON Sajda
instance FromJSON Sajda

-- | Complete Quran metadata with all 7 organizational systems
data QuranMetadataFull = QuranMetadataFull
  { juzs :: ![Juz]
  , quarters :: ![Quarter]
  , manzils :: ![Manzil]
  , rukus :: ![Ruku]
  , pages :: ![Page]
  , sajdas :: ![Sajda]
  } deriving (Eq, Show, Generic)

instance ToJSON QuranMetadataFull
instance FromJSON QuranMetadataFull

-- | Parse quran-data.xml file
parseQuranMetadataFile :: FilePath -> IO (Either String QuranMetadataFull)
parseQuranMetadataFile filepath = do
  content <- TIO.readFile filepath
  return $ parseQuranMetadataXML content

-- | Parse quran-data.xml content
parseQuranMetadataXML :: Text -> Either String QuranMetadataFull
parseQuranMetadataXML content =
  case parseXMLDoc (T.unpack content) of
    Nothing -> Left "Failed to parse XML"
    Just root -> parseMetadata root

-- | Extract metadata from XML root element
parseMetadata :: Element -> Either String QuranMetadataFull
parseMetadata root = do
  -- Find each section in the XML
  let juzList = parseJuzs root
      quarterList = parseQuarters root
      manzilList = parseManzils root
      rukuList = parseRukus root
      pageList = parsePages root
      sajdaList = parseSajdas root

  -- Validate counts
  if length juzList /= 30 then
    Left $ "Expected 30 Juz, found " ++ show (length juzList)
  else if length quarterList /= 240 then
    Left $ "Expected 240 Quarters, found " ++ show (length quarterList)
  else if length manzilList /= 7 then
    Left $ "Expected 7 Manzils, found " ++ show (length manzilList)
  else if length rukuList /= 556 then
    Left $ "Expected 556 Rukus, found " ++ show (length rukuList)
  else if length pageList /= 604 then
    Left $ "Expected 604 Pages, found " ++ show (length pageList)
  else if length sajdaList /= 15 then
    Left $ "Expected 15 Sajdas, found " ++ show (length sajdaList)
  else
    Right QuranMetadataFull
      { juzs = juzList
      , quarters = quarterList
      , manzils = manzilList
      , rukus = rukuList
      , pages = pageList
      , sajdas = sajdaList
      }

-- | Parse Juz elements
parseJuzs :: Element -> [Juz]
parseJuzs root =
  case findChild (QName "juzs" Nothing Nothing) root of
    Nothing -> []
    Just juzs -> mapMaybe parseJuz (findChildren (QName "juz" Nothing Nothing) juzs)

parseJuz :: Element -> Maybe Juz
parseJuz el = do
  index <- readAttr "index" el
  sura <- readAttr "sura" el
  aya <- readAttr "aya" el
  return Juz
    { juzIndex = index
    , juzStartSurah = sura
    , juzStartAya = aya
    }

-- | Parse Quarter elements (from hizbs section)
parseQuarters :: Element -> [Quarter]
parseQuarters root =
  case findChild (QName "hizbs" Nothing Nothing) root of
    Nothing -> []
    Just hizbs -> mapMaybe parseQuarter (findChildren (QName "quarter" Nothing Nothing) hizbs)

parseQuarter :: Element -> Maybe Quarter
parseQuarter el = do
  index <- readAttr "index" el
  sura <- readAttr "sura" el
  aya <- readAttr "aya" el
  return Quarter
    { quarterIndex = index
    , quarterStartSurah = sura
    , quarterStartAya = aya
    }

-- | Parse Manzil elements
parseManzils :: Element -> [Manzil]
parseManzils root =
  case findChild (QName "manzils" Nothing Nothing) root of
    Nothing -> []
    Just manzils -> mapMaybe parseManzil (findChildren (QName "manzil" Nothing Nothing) manzils)

parseManzil :: Element -> Maybe Manzil
parseManzil el = do
  index <- readAttr "index" el
  sura <- readAttr "sura" el
  aya <- readAttr "aya" el
  return Manzil
    { manzilIndex = index
    , manzilStartSurah = sura
    , manzilStartAya = aya
    }

-- | Parse Ruku elements (MOST IMPORTANT - thematic sections)
parseRukus :: Element -> [Ruku]
parseRukus root =
  case findChild (QName "rukus" Nothing Nothing) root of
    Nothing -> []
    Just rukus -> mapMaybe parseRuku (findChildren (QName "ruku" Nothing Nothing) rukus)

parseRuku :: Element -> Maybe Ruku
parseRuku el = do
  index <- readAttr "index" el
  sura <- readAttr "sura" el
  aya <- readAttr "aya" el
  return Ruku
    { rukuIndex = index
    , rukuSurah = sura
    , rukuStartAya = aya
    }

-- | Parse Page elements
parsePages :: Element -> [Page]
parsePages root =
  case findChild (QName "pages" Nothing Nothing) root of
    Nothing -> []
    Just pages -> mapMaybe parsePage (findChildren (QName "page" Nothing Nothing) pages)

parsePage :: Element -> Maybe Page
parsePage el = do
  index <- readAttr "index" el
  sura <- readAttr "sura" el
  aya <- readAttr "aya" el
  return Page
    { pageIndex = index
    , pageStartSurah = sura
    , pageStartAya = aya
    }

-- | Parse Sajda elements
parseSajdas :: Element -> [Sajda]
parseSajdas root =
  case findChild (QName "sajdas" Nothing Nothing) root of
    Nothing -> []
    Just sajdas -> mapMaybe parseSajda (findChildren (QName "sajda" Nothing Nothing) sajdas)

parseSajda :: Element -> Maybe Sajda
parseSajda el = do
  index <- readAttr "index" el
  sura <- readAttr "sura" el
  aya <- readAttr "aya" el
  typeStr <- readAttrText "type" el
  sajdaType' <- parseSajdaType typeStr
  return Sajda
    { sajdaIndex = index
    , sajdaSurah = sura
    , sajdaAya = aya
    , sajdaType = sajdaType'
    }

parseSajdaType :: Text -> Maybe SajdaType
parseSajdaType "obligatory" = Just Obligatory
parseSajdaType "recommended" = Just Recommended
parseSajdaType _ = Nothing

-- | Helper to read integer attribute
readAttr :: String -> Element -> Maybe Int
readAttr name el = do
  attrVal <- findAttr (QName name Nothing Nothing) el
  case reads attrVal of
    [(n, "")] -> Just n
    _ -> Nothing

-- | Helper to read text attribute
readAttrText :: String -> Element -> Maybe Text
readAttrText name el = T.pack <$> findAttr (QName name Nothing Nothing) el

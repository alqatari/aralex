{-|
Module      : Parser.LetterMeanings
Description : JSON parser for Arabic letter meanings
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Parses JSON file containing phonosemantic meanings for 28 Arabic letters
based on Hassan Abbas's theory (خصائص الحروف العربية ومعانيها).
-}

module Parser.LetterMeanings
  ( -- * Types
    LetterMeaningJSON(..)
    -- * Parsing
  , parseLetterMeanings
  , parseLetterMeaningsFile
    -- * Loading
  , loadLetterMeanings
  ) where

import Data.Aeson (FromJSON(..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import GHC.Generics (Generic)

import Domain.Phonosemantics

-- | JSON representation of letter meaning (matches arabic_letters.json schema)
data LetterMeaningJSON = LetterMeaningJSON
  { letter              :: Text
  , letterNameAr        :: Text
  , category            :: Text
  , articulationPoint   :: Text
  , primaryMeaningAr    :: Text
  , primaryMeaningEn    :: Maybe Text
  , semanticDomains     :: [Text]
  , attributes          :: [Text]
  , examples            :: [Text]
  , sourceReference     :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LetterMeaningJSON where
  parseJSON = Aeson.withObject "LetterMeaningJSON" $ \o -> do
    letter' <- o .: "letter"
    letterNameAr' <- o .: "letter_name_ar"
    category' <- o .: "category"
    articulationPoint' <- o .: "articulation_point"
    primaryMeaningAr' <- o .: "primary_meaning_ar"
    primaryMeaningEn' <- o .: "primary_meaning_en"
    semanticDomains' <- o .: "semantic_domains"
    attributes' <- o .: "attributes"
    examples' <- o .: "examples"
    sourceReference' <- o .: "source_reference"

    pure $ LetterMeaningJSON
      { letter = letter'
      , letterNameAr = letterNameAr'
      , category = category'
      , articulationPoint = articulationPoint'
      , primaryMeaningAr = primaryMeaningAr'
      , primaryMeaningEn = primaryMeaningEn'
      , semanticDomains = semanticDomains'
      , attributes = attributes'
      , examples = examples'
      , sourceReference = sourceReference'
      }

-- | Parse letter meanings from JSON ByteString
parseLetterMeanings :: BL.ByteString -> Either String [LetterMeaningJSON]
parseLetterMeanings = Aeson.eitherDecode

-- | Parse letter meanings file by path
parseLetterMeaningsFile :: FilePath -> IO (Either String [LetterMeaningJSON])
parseLetterMeaningsFile path = do
  content <- BL.readFile path
  pure $ parseLetterMeanings content

-- | Load letter meanings from data directory
loadLetterMeanings :: FilePath -> IO (Either String [LetterMeaningJSON])
loadLetterMeanings dataDir = do
  let fullPath = dataDir </> "letter_meanings" </> "arabic_letters.json"
  parseLetterMeaningsFile fullPath

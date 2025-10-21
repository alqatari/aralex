{-|
Module      : Parser.Dictionary
Description : JSON parser for dictionary entries
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Parses JSON files containing dictionary entries from 7 classical dictionaries.
-}

module Parser.Dictionary
  ( -- * Parsing
    parseDictFile
  , parseDictEntries
    -- * Loading
  , loadDictionaryEntries
  , loadAllDictionaries
    -- * Utilities
  , countEntries
  , validateEntries
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import Domain.Dictionary

-- | Parse dictionary entries from JSON ByteString
parseDictEntries :: BL.ByteString -> Either String [DictEntry]
parseDictEntries = eitherDecode

-- | Parse dictionary file by path
parseDictFile :: FilePath -> IO (Either String [DictEntry])
parseDictFile path = do
  content <- BL.readFile path
  pure $ parseDictEntries content

-- | Load dictionary entries from data directory
loadDictionaryEntries :: FilePath -> DictionaryId -> IO (Either String [DictEntry])
loadDictionaryEntries dataDir dictId = do
  let filename = getDictFilename dictId
      fullPath = dataDir </> "arabic_dicts" </> T.unpack filename
  parseDictFile fullPath

-- | Get filename for dictionary ID
getDictFilename :: DictionaryId -> Text
getDictFilename dId =
  case filter (\m -> dId == dictId m) dictionaryMetadata of
    (meta:_) -> filename meta
    []       -> error $ "No metadata for dictionary: " <> show dId

-- | Load all 7 dictionaries
loadAllDictionaries :: FilePath -> IO (Either String [(DictionaryId, [DictEntry])])
loadAllDictionaries dataDir = do
  results <- mapM loadOne [minBound .. maxBound]
  pure $ sequence results
  where
    loadOne :: DictionaryId -> IO (Either String (DictionaryId, [DictEntry]))
    loadOne dictId = do
      result <- loadDictionaryEntries dataDir dictId
      pure $ case result of
        Left err      -> Left $ show dictId <> ": " <> err
        Right entries -> Right (dictId, entries)

-- | Count entries in parsed data
countEntries :: [DictEntry] -> Int
countEntries = length

-- | Validate parsed entries
validateEntries :: [DictEntry] -> Either String ()
validateEntries entries = do
  when (null entries) $
    Left "No entries found in dictionary file"

  -- Check for empty roots
  let emptyRoots = filter (T.null . unRoot . root) entries
  when (not $ null emptyRoots) $
    Left $ "Found " <> show (length emptyRoots) <> " entries with empty roots"

  -- Check for empty definitions
  let emptyDefs = filter (T.null . definitionArabic) entries
  when (not $ null emptyDefs) $
    Left $ "Found " <> show (length emptyDefs) <> " entries with empty definitions"

  pure ()

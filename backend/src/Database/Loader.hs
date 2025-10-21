{-|
Module      : Database.Loader
Description : Database loaders for all data sources
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Load parsed data into SQLite database.
-}

module Database.Loader
  ( -- * Dictionary Loading (aradicts.db)
    loadDictionarySources
  , loadDictionaryEntries
    -- * Quran Loading (aralex.db)
  , loadQuranicWords
  , loadVerses
  , loadQuranMetadata
    -- * Letter Meanings Loading (aralex.db)
  , loadLetterMeanings
    -- * Batch Operations
  , loadAllData
  ) where

import Control.Monad (forM_)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, defaultTimeLocale)
import Database.SQLite.Simple
import System.IO (hPutStrLn, stderr)

import Database.Schema (DatabaseConnections(..))
import Domain.Dictionary
import Domain.Morphology
import Domain.MorphologyDTO (segmentToDTO, wordToDTO)
import Domain.Verse
import qualified Parser.Dictionary as PD
import qualified Parser.Morphology as PM
import qualified Parser.Verse as PV
import qualified Parser.QuranMetadata as QM
import qualified Parser.LetterMeanings as PLM
import Parser.QuranMetadata (QuranMetadataFull(..), Juz(..), Quarter(..), Manzil(..), Ruku(..), Page(..), Sajda(..), SajdaType(..))
import Parser.LetterMeanings (LetterMeaningJSON(..))

-- | Load dictionary source metadata
loadDictionarySources :: Connection -> IO ()
loadDictionarySources conn = do
  hPutStrLn stderr "Loading dictionary sources..."
  forM_ dictionaryMetadata $ \meta -> do
    execute conn
      "INSERT OR REPLACE INTO dictionary_sources (id, dict_id, author, title, death_year, entry_count, filename) VALUES (?, ?, ?, ?, ?, ?, ?)"
      ( fromEnum (dictId meta) + 1 :: Int
      , show (dictId meta) :: String
      , author meta :: Text
      , title meta :: Text
      , deathYear meta :: Int
      , entryCount meta :: Int
      , filename meta :: Text
      )
  hPutStrLn stderr $ "✅ Loaded " <> show (length dictionaryMetadata) <> " dictionary sources"

-- | Load dictionary entries
loadDictionaryEntries :: Connection -> DictionaryId -> [DictEntry] -> IO ()
loadDictionaryEntries conn dictId entries = do
  let sourceId = fromEnum dictId + 1
  hPutStrLn stderr $ "Loading " <> show (length entries) <> " entries for " <> show dictId <> "..."

  -- Batch insert with transaction (lightweight - only 3 fields)
  execute_ conn "BEGIN TRANSACTION"
  forM_ entries $ \entry -> do
    execute conn
      "INSERT INTO dictionary_entries (dict_source_id, root, root_normalized, definition_arabic) VALUES (?, ?, ?, ?)"
      ( sourceId :: Int
      , unRoot (root entry) :: Text
      , unRoot (rootNormalized entry) :: Text
      , definitionArabic entry :: Text
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length entries) <> " entries"

-- | Load Quranic words (aggregated morphology)
loadQuranicWords :: Connection -> [QuranicWord] -> IO ()
loadQuranicWords conn qWords = do
  hPutStrLn stderr $ "Loading " <> show (length qWords) <> " Quranic words..."

  execute_ conn "BEGIN TRANSACTION"
  forM_ qWords $ \word -> do
    let wloc = wordLocation word
        segmentsJson = BL.toStrict $ encode $ map segmentToDTO (segments word)
    execute conn
      "INSERT INTO quranic_words (surah, verse, word_position, surface_form, root, lemma, pos, segments_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      ( wSurah wloc :: Int
      , wVerse wloc :: Int
      , wWord wloc :: Int
      , fullSurface word :: Text
      , wordRoot word :: Maybe Text
      , wordLemma word :: Maybe Text
      , show (wordPOS word) :: String
      , segmentsJson
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length qWords) <> " words"

-- | Load verse text
loadVerses :: Connection -> [VerseText] -> IO ()
loadVerses conn verses = do
  hPutStrLn stderr $ "Loading " <> show (length verses) <> " verses..."

  execute_ conn "BEGIN TRANSACTION"
  forM_ verses $ \verse -> do
    let ref = verseRef verse
    execute conn
      "INSERT INTO verses (surah, verse, text, word_count) VALUES (?, ?, ?, ?)"
      ( refSurah ref :: Int
      , refVerse ref :: Int
      , text verse :: Text
      , wordCount verse :: Int
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length verses) <> " verses"

-- | Load Quran metadata (juz, quarters, manzils, rukus, pages, sajdas)
loadQuranMetadata :: Connection -> QuranMetadataFull -> IO ()
loadQuranMetadata conn metadata = do
  hPutStrLn stderr "Loading Quran metadata..."

  -- Load Juz (30 parts)
  execute_ conn "BEGIN TRANSACTION"
  forM_ (juzs metadata) $ \juz -> do
    execute conn
      "INSERT INTO juz (juz_index, start_surah, start_aya) VALUES (?, ?, ?)"
      ( juzIndex juz :: Int
      , juzStartSurah juz :: Int
      , juzStartAya juz :: Int
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length $ juzs metadata) <> " juz"

  -- Load Quarters (240 hizb quarters)
  execute_ conn "BEGIN TRANSACTION"
  forM_ (quarters metadata) $ \quarter -> do
    execute conn
      "INSERT INTO quarters (quarter_index, start_surah, start_aya) VALUES (?, ?, ?)"
      ( quarterIndex quarter :: Int
      , quarterStartSurah quarter :: Int
      , quarterStartAya quarter :: Int
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length $ quarters metadata) <> " quarters"

  -- Load Manzils (7 stations)
  execute_ conn "BEGIN TRANSACTION"
  forM_ (manzils metadata) $ \manzil -> do
    execute conn
      "INSERT INTO manzils (manzil_index, start_surah, start_aya) VALUES (?, ?, ?)"
      ( manzilIndex manzil :: Int
      , manzilStartSurah manzil :: Int
      , manzilStartAya manzil :: Int
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length $ manzils metadata) <> " manzils"

  -- Load Rukus (558 thematic sections) - MOST IMPORTANT
  execute_ conn "BEGIN TRANSACTION"
  forM_ (rukus metadata) $ \ruku -> do
    execute conn
      "INSERT INTO rukus (ruku_index, surah, start_aya, theme) VALUES (?, ?, ?, ?)"
      ( rukuIndex ruku :: Int
      , rukuSurah ruku :: Int
      , rukuStartAya ruku :: Int
      , Nothing :: Maybe Text  -- Theme will be added manually/ML later
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length $ rukus metadata) <> " rukus (thematic sections)"

  -- Load Pages (604 Madina Mushaf pages)
  execute_ conn "BEGIN TRANSACTION"
  forM_ (pages metadata) $ \page -> do
    execute conn
      "INSERT INTO pages (page_index, start_surah, start_aya) VALUES (?, ?, ?)"
      ( pageIndex page :: Int
      , pageStartSurah page :: Int
      , pageStartAya page :: Int
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length $ pages metadata) <> " pages"

  -- Load Sajdas (15 prostration verses)
  execute_ conn "BEGIN TRANSACTION"
  forM_ (sajdas metadata) $ \sajda -> do
    execute conn
      "INSERT INTO sajdas (sajda_index, surah, aya, sajda_type) VALUES (?, ?, ?, ?)"
      ( sajdaIndex sajda :: Int
      , sajdaSurah sajda :: Int
      , sajdaAya sajda :: Int
      , show (sajdaType sajda) :: String
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length $ sajdas metadata) <> " sajdas"

-- | Load letter meanings (phonosemantic data)
loadLetterMeanings :: Connection -> [LetterMeaningJSON] -> IO ()
loadLetterMeanings conn letterMeanings = do
  hPutStrLn stderr $ "Loading " <> show (length letterMeanings) <> " letter meanings..."

  execute_ conn "BEGIN TRANSACTION"
  forM_ letterMeanings $ \lm -> do
    -- Convert lists to JSON strings for storage
    let domainsJson = T.pack $ show $ semanticDomains lm
        attributesJson = T.pack $ show $ attributes lm
        examplesJson = T.pack $ show $ examples lm
    execute conn
      "INSERT INTO letter_meanings (letter, letter_name_ar, category, articulation_point, primary_meaning_ar, primary_meaning_en, semantic_domains, attributes, examples, source_reference) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( letter lm :: Text
      , letterNameAr lm :: Text
      , category lm :: Text
      , articulationPoint lm :: Text
      , primaryMeaningAr lm :: Text
      , primaryMeaningEn lm :: Maybe Text
      , domainsJson :: Text
      , attributesJson :: Text
      , examplesJson :: Text
      , sourceReference lm :: Text
      )
  execute_ conn "COMMIT"
  hPutStrLn stderr $ "✅ Loaded " <> show (length letterMeanings) <> " letter meanings"

-- | Load all data from data directory into both databases
loadAllData :: DatabaseConnections -> FilePath -> IO ()
loadAllData (DatabaseConnections dictConn' quranConn') dataDir = do
  hPutStrLn stderr "\n=== Starting Full Data Load (Two-Database Architecture) ==="
  hPutStrLn stderr $ "Data directory: " <> dataDir

  -- 1. Load dictionary sources into aradicts.db
  hPutStrLn stderr "\n📚 Loading dictionaries into aradicts.db..."
  loadDictionarySources dictConn'

  -- 2. Load all dictionaries into aradicts.db
  result <- PD.loadAllDictionaries dataDir
  case result of
    Left err -> hPutStrLn stderr $ "❌ Error loading dictionaries: " <> err
    Right dictData -> do
      forM_ dictData $ \(dictId, entries) -> do
        loadDictionaryEntries dictConn' dictId entries
      hPutStrLn stderr $ "✅ Loaded all " <> show (sum $ map (length . snd) dictData) <> " dictionary entries"

  -- 3. Load morphology into aralex.db
  hPutStrLn stderr "\n📖 Loading Quranic morphology into aralex.db..."
  morphResult <- PM.loadMorphologyData dataDir
  case morphResult of
    Left err -> hPutStrLn stderr $ "❌ Error loading morphology: " <> err
    Right segments -> do
      hPutStrLn stderr $ "Parsed " <> show (length segments) <> " morpheme segments"
      let qWords = aggregateSegments segments
      hPutStrLn stderr $ "Aggregated into " <> show (length qWords) <> " words"
      loadQuranicWords quranConn' qWords

  -- 4. Load verses into aralex.db
  hPutStrLn stderr "\n📜 Loading verse text into aralex.db..."
  verseResult <- PV.loadVerseText dataDir
  case verseResult of
    Left err -> hPutStrLn stderr $ "❌ Error loading verses: " <> err
    Right verses -> loadVerses quranConn' verses

  -- 5. Load Quran metadata into aralex.db
  hPutStrLn stderr "\n🗂️  Loading Quran metadata (juz, rukus, pages, etc.) into aralex.db..."
  metadataResult <- QM.parseQuranMetadataFile (dataDir <> "/quran_text/quran-data.xml")
  case metadataResult of
    Left err -> hPutStrLn stderr $ "❌ Error loading metadata: " <> err
    Right metadata -> loadQuranMetadata quranConn' metadata

  -- 6. Load letter meanings into aralex.db
  hPutStrLn stderr "\n🔤 Loading letter meanings (phonosemantic theory) into aralex.db..."
  letterMeaningsResult <- PLM.loadLetterMeanings dataDir
  case letterMeaningsResult of
    Left err -> hPutStrLn stderr $ "❌ Error loading letter meanings: " <> err
    Right letterMeanings -> loadLetterMeanings quranConn' letterMeanings

  hPutStrLn stderr "\n✅ Data load complete!"
  hPutStrLn stderr "  - aradicts.db: Dictionary sources + entries"
  hPutStrLn stderr "  - aralex.db: Quranic morphology + verses + metadata + letter meanings"

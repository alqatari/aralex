{-|
Module      : Database.Schema
Description : SQLite database schema for aralex (two-database architecture)
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Database schema split into two databases:
- aradicts.db: Classical dictionaries (6 sources, 30,310 entries, deduplicated)
- aralex.db: Quranic corpus (morphology + verse text)
-}

module Database.Schema
  ( -- * Schema Creation
    createDictSchema
  , createQuranSchema
  , initDictDatabase
  , initQuranDatabase
  , initBothDatabases
    -- * Table Names
  , dictionarySourcesTable
  , dictionaryEntriesTable
  , quranicWordsTable
  , versesTable
    -- * Connection Pair
  , DatabaseConnections(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import qualified Database.SQLite3 as SQLite3

-- | Table names
dictionarySourcesTable :: String
dictionarySourcesTable = "dictionary_sources"

dictionaryEntriesTable :: String
dictionaryEntriesTable = "dictionary_entries"

quranicWordsTable :: String
quranicWordsTable = "quranic_words"

versesTable :: String
versesTable = "verses"

-- | Pair of database connections (separation of concerns)
data DatabaseConnections = DatabaseConnections
  { dictConn  :: Connection  -- ^ aradicts.db connection
  , quranConn :: Connection  -- ^ aralex.db connection
  }

-- | SQL schema for dictionary database (aradicts.db)
dictSchemaSQL :: Text
dictSchemaSQL = T.pack $ unlines
  [ "CREATE TABLE IF NOT EXISTS dictionary_sources ("
  , "  id INTEGER PRIMARY KEY,"
  , "  dict_id TEXT NOT NULL UNIQUE,"
  , "  author TEXT NOT NULL,"
  , "  title TEXT NOT NULL,"
  , "  death_year INTEGER NOT NULL,"
  , "  entry_count INTEGER NOT NULL,"
  , "  filename TEXT NOT NULL"
  , ");"
  , ""
  , "CREATE TABLE IF NOT EXISTS dictionary_entries ("
  , "  id INTEGER PRIMARY KEY,"
  , "  dict_source_id INTEGER NOT NULL,"
  , "  root TEXT NOT NULL,"
  , "  root_normalized TEXT NOT NULL,"
  , "  definition_arabic TEXT NOT NULL,"
  , "  FOREIGN KEY (dict_source_id) REFERENCES dictionary_sources(id)"
  , ");"
  , ""
  , "CREATE INDEX IF NOT EXISTS idx_root_normalized ON dictionary_entries(root_normalized);"
  , "CREATE INDEX IF NOT EXISTS idx_dict_source ON dictionary_entries(dict_source_id);"
  ]

-- | SQL schema for Quran database (aralex.db)
quranSchemaSQL :: Text
quranSchemaSQL = T.pack $ unlines
  [ "CREATE TABLE IF NOT EXISTS quranic_words ("
  , "  id INTEGER PRIMARY KEY,"
  , "  surah INTEGER NOT NULL,"
  , "  verse INTEGER NOT NULL,"
  , "  word_position INTEGER NOT NULL,"
  , "  surface_form TEXT NOT NULL,"
  , "  root TEXT,"
  , "  lemma TEXT,"
  , "  pos TEXT NOT NULL,"
  , "  segments_json TEXT NOT NULL,"
  , "  UNIQUE(surah, verse, word_position)"
  , ");"
  , ""
  , "CREATE INDEX IF NOT EXISTS idx_word_root ON quranic_words(root);"
  , "CREATE INDEX IF NOT EXISTS idx_word_verse ON quranic_words(surah, verse);"
  , ""
  , "CREATE TABLE IF NOT EXISTS verses ("
  , "  id INTEGER PRIMARY KEY,"
  , "  surah INTEGER NOT NULL,"
  , "  verse INTEGER NOT NULL,"
  , "  text TEXT NOT NULL,"
  , "  word_count INTEGER NOT NULL,"
  , "  UNIQUE(surah, verse)"
  , ");"
  , ""
  , "CREATE INDEX IF NOT EXISTS idx_verse_ref ON verses(surah, verse);"
  , ""
  , "-- Quran Metadata Tables (from quran-data.xml)"
  , "CREATE TABLE IF NOT EXISTS juz ("
  , "  juz_index INTEGER PRIMARY KEY,"
  , "  start_surah INTEGER NOT NULL,"
  , "  start_aya INTEGER NOT NULL"
  , ");"
  , ""
  , "CREATE TABLE IF NOT EXISTS quarters ("
  , "  quarter_index INTEGER PRIMARY KEY,"
  , "  start_surah INTEGER NOT NULL,"
  , "  start_aya INTEGER NOT NULL"
  , ");"
  , ""
  , "CREATE TABLE IF NOT EXISTS manzils ("
  , "  manzil_index INTEGER PRIMARY KEY,"
  , "  start_surah INTEGER NOT NULL,"
  , "  start_aya INTEGER NOT NULL"
  , ");"
  , ""
  , "CREATE TABLE IF NOT EXISTS rukus ("
  , "  ruku_index INTEGER PRIMARY KEY,"
  , "  surah INTEGER NOT NULL,"
  , "  start_aya INTEGER NOT NULL,"
  , "  theme TEXT"
  , ");"
  , ""
  , "CREATE INDEX IF NOT EXISTS idx_ruku_surah ON rukus(surah);"
  , ""
  , "CREATE TABLE IF NOT EXISTS pages ("
  , "  page_index INTEGER PRIMARY KEY,"
  , "  start_surah INTEGER NOT NULL,"
  , "  start_aya INTEGER NOT NULL"
  , ");"
  , ""
  , "CREATE TABLE IF NOT EXISTS sajdas ("
  , "  sajda_index INTEGER PRIMARY KEY,"
  , "  surah INTEGER NOT NULL,"
  , "  aya INTEGER NOT NULL,"
  , "  sajda_type TEXT NOT NULL CHECK(sajda_type IN ('Obligatory', 'Recommended'))"
  , ");"
  , ""
  , "-- Letter Meanings Table (Hassan Abbas phonosemantic theory)"
  , "CREATE TABLE IF NOT EXISTS letter_meanings ("
  , "  id INTEGER PRIMARY KEY,"
  , "  letter TEXT NOT NULL UNIQUE,"
  , "  letter_name_ar TEXT NOT NULL,"
  , "  category TEXT NOT NULL,"
  , "  articulation_point TEXT NOT NULL,"
  , "  primary_meaning_ar TEXT NOT NULL,"
  , "  primary_meaning_en TEXT,"
  , "  semantic_domains TEXT NOT NULL,"
  , "  attributes TEXT NOT NULL,"
  , "  examples TEXT NOT NULL,"
  , "  source_reference TEXT"
  , ");"
  , ""
  , "CREATE INDEX IF NOT EXISTS idx_letter ON letter_meanings(letter);"
  ]

-- | Create dictionary database schema (aradicts.db)
createDictSchema :: Connection -> IO ()
createDictSchema conn = do
  let rawConn = connectionHandle conn
  SQLite3.exec rawConn dictSchemaSQL

-- | Create Quran database schema (aralex.db)
createQuranSchema :: Connection -> IO ()
createQuranSchema conn = do
  let rawConn = connectionHandle conn
  SQLite3.exec rawConn quranSchemaSQL

-- | Initialize dictionary database (aradicts.db)
initDictDatabase :: FilePath -> IO Connection
initDictDatabase dbPath = do
  conn <- open dbPath
  createDictSchema conn
  pure conn

-- | Initialize Quran database (aralex.db)
initQuranDatabase :: FilePath -> IO Connection
initQuranDatabase dbPath = do
  conn <- open dbPath
  createQuranSchema conn
  pure conn

-- | Initialize both databases with proper separation of concerns
initBothDatabases :: FilePath -> FilePath -> IO DatabaseConnections
initBothDatabases dictDbPath quranDbPath = do
  dictConn' <- initDictDatabase dictDbPath
  quranConn' <- initQuranDatabase quranDbPath
  pure $ DatabaseConnections dictConn' quranConn'

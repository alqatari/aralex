{-# LANGUAGE TypeOperators #-}

{-|
Module      : API.Routes
Description : Servant API routes
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

API endpoints for querying dictionaries, morphology, and verses.
-}

module API.Routes
  ( API
  , api
  , server
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant

import qualified API.Handlers as H
import qualified API.EtymologyGraph as EG
import Database.Schema (DatabaseConnections(..))
import Domain.Dictionary (DictEntry, RootText(..))
import Domain.MorphologyDTO (QuranicWordDTO)
import Domain.Verse (VerseRef, VerseText, VerseWithRoot)
import Domain.Phonosemantics (WordAnalysis)
import Domain.EtymologyGraph (EtymologyGraph)

-- | API type
type API =
       "api" :> "v1" :>
       (    "dictionaries" :> Get '[JSON] [Text]
       :<|> "dictionary" :> Capture "root" Text :> Get '[JSON] [DictEntry]
       :<|> "verses" :> Capture "root" Text :> Get '[JSON] [VerseWithRoot]
       :<|> "morphology" :> Capture "surah" Int :> Capture "verse" Int :> Get '[JSON] [QuranicWordDTO]
       :<|> "verse" :> Capture "surah" Int :> Capture "verse" Int :> Get '[JSON] VerseText
       :<|> "analyze" :> Capture "word" Text :> Get '[JSON] WordAnalysis
       :<|> "letters" :> Get '[JSON] [(Text, Text, Text, Text)]
       :<|> "letter" :> Capture "char" Text :> Get '[JSON] (Maybe (Text, Text, Text, Text, Text, Text, Text, Text, Text, Text))
       :<|> "etymology-graph" :> Capture "root" Text :> Get '[JSON] EtymologyGraph
       )

api :: Proxy API
api = Proxy

-- | Servant server implementation with two database connections
server :: DatabaseConnections -> Server API
server dbConns@(DatabaseConnections dictConn' quranConn') =
       getDictionaries
  :<|> getDictionaryEntry
  :<|> getVersesByRoot
  :<|> getMorphology
  :<|> getVerse
  :<|> analyzeWord
  :<|> getAllLetters
  :<|> getLetterMeaning
  :<|> getEtymologyGraph
  where
    getDictionaries :: Handler [Text]
    getDictionaries = pure ["ain", "sihah", "maqayis", "muhkam", "mufradat", "lisan", "qamus"]

    getDictionaryEntry :: Text -> Handler [DictEntry]
    getDictionaryEntry rootText = liftIO $ H.lookupRoot dictConn' (RootText rootText)

    getVersesByRoot :: Text -> Handler [VerseWithRoot]
    getVersesByRoot searchText = liftIO $ H.lookupVersesByWord quranConn' searchText

    getMorphology :: Int -> Int -> Handler [QuranicWordDTO]
    getMorphology surahNum verseNum = liftIO $ H.lookupMorphology quranConn' surahNum verseNum

    getVerse :: Int -> Int -> Handler VerseText
    getVerse surahNum verseNum = do
      result <- liftIO $ H.lookupVerse quranConn' surahNum verseNum
      case result of
        Just v  -> pure v
        Nothing -> throwError err404

    analyzeWord :: Text -> Handler WordAnalysis
    analyzeWord word = liftIO $ H.performAnalysis (DatabaseConnections dictConn' quranConn') word  -- Uses both DBs

    getAllLetters :: Handler [(Text, Text, Text, Text)]
    getAllLetters = liftIO $ H.getAllLetters quranConn'  -- Letter meanings in aralex.db

    getLetterMeaning :: Text -> Handler (Maybe (Text, Text, Text, Text, Text, Text, Text, Text, Text, Text))
    getLetterMeaning letterChar = liftIO $ H.getLetterMeaning quranConn' letterChar

    getEtymologyGraph :: Text -> Handler EtymologyGraph
    getEtymologyGraph rootText = liftIO $ EG.buildEtymologyGraph dbConns (RootText rootText)

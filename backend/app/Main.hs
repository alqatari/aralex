{-|
Module      : Main
Description : Main entry point for aralex backend server
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Runs Servant API server with WAI/Warp.
Initializes database and loads data on first run.
-}

module Main (main) where

import Data.Text (pack)
import Network.Wai (Application, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Servant (serve)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import WaiAppStatic.Types (ssIndices, unsafeToPiece)

import API.Routes (api, server)
import Database.Schema (initBothDatabases)
import Database.Loader (loadAllData)

-- | Main entry point
main :: IO ()
main = do
  let dictDbPath = "aradicts.db"  -- Dictionary database
      quranDbPath = "aralex.db"    -- Quran corpus database
      dataDir = "../data"           -- Relative to backend/

  hPutStrLn stderr "=== Aralex Backend Server (Two-Database Architecture) ==="

  -- Check if databases exist
  dictDbExists <- doesFileExist dictDbPath
  quranDbExists <- doesFileExist quranDbPath

  -- Initialize both databases
  dbConns <- initBothDatabases dictDbPath quranDbPath

  -- Load data if databases are new
  if not dictDbExists || not quranDbExists
    then do
      hPutStrLn stderr "📦 New databases detected - loading all data..."
      hPutStrLn stderr "  - aradicts.db: Dictionary sources + entries"
      hPutStrLn stderr "  - aralex.db: Quranic morphology + verses"
      loadAllData dbConns dataDir
      hPutStrLn stderr "✅ Databases initialized successfully!"
    else do
      hPutStrLn stderr "✅ Using existing databases:"
      hPutStrLn stderr "  - aradicts.db: Dictionary data"
      hPutStrLn stderr "  - aralex.db: Quran corpus data"

  -- Start server with frontend serving
  let port = 8080
      frontendPath = "../frontend"  -- Serve from frontend root (contains both public/ and output/)

      -- Serve API and frontend
      app :: Application
      app = simpleCors $ \req respond -> do
        let path = pathInfo req
        case path of
          -- API routes
          (seg : _) | seg == pack "api" -> serve api (server dbConns) req respond
          -- Root path: serve index.html from public/
          [] -> staticApp (publicSettings frontendPath) req respond
          -- Everything else: try to serve from frontend root
          _ -> staticApp (frontendSettings frontendPath) req respond

      -- Settings for serving public/ folder (has index.html)
      publicSettings fp =
        let settings = defaultFileServerSettings (fp ++ "/public")
        in settings { ssIndices = [unsafeToPiece $ pack "index.html"] }

      -- Settings for serving all frontend files (no caching in dev mode)
      frontendSettings fp =
        defaultFileServerSettings fp

  hPutStrLn stderr ""
  hPutStrLn stderr $ "🚀 Starting server on port " <> show port
  hPutStrLn stderr $ "📡 API: http://localhost:" <> show port <> "/api/v1"
  hPutStrLn stderr $ "🌐 Frontend: http://localhost:" <> show port
  hPutStrLn stderr ""
  hPutStrLn stderr "Available API endpoints:"
  hPutStrLn stderr "  GET  /api/v1/dictionaries"
  hPutStrLn stderr "  GET  /api/v1/dictionary/:root       (aradicts.db)"
  hPutStrLn stderr "  GET  /api/v1/morphology/:surah/:verse (aralex.db)"
  hPutStrLn stderr "  GET  /api/v1/verse/:surah/:verse      (aralex.db)"
  hPutStrLn stderr "  GET  /api/v1/analyze/:word"
  hPutStrLn stderr ""

  run port app

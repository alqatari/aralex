module Main where

import Parser.QuranMetadata
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let xmlPath = "../data/quran_text/quran-data.xml"
  putStrLn $ "Parsing " ++ xmlPath ++ "..."
  putStrLn ""

  result <- parseQuranMetadataFile xmlPath
  case result of
    Left err -> do
      putStrLn $ "❌ Error: " ++ err
      exitFailure
    Right metadata -> do
      putStrLn "✅ Successfully parsed quran-data.xml"
      putStrLn ""
      putStrLn $ "  📊 Juz: " ++ show (length $ juzs metadata) ++ " parts (expected 30)"
      putStrLn $ "  📊 Quarters: " ++ show (length $ quarters metadata) ++ " divisions (expected 240)"
      putStrLn $ "  📊 Manzils: " ++ show (length $ manzils metadata) ++ " stations (expected 7)"
      putStrLn $ "  📊 Rukus: " ++ show (length $ rukus metadata) ++ " sections (expected 556) ⭐"
      putStrLn $ "  📊 Pages: " ++ show (length $ pages metadata) ++ " pages (expected 604)"
      putStrLn $ "  📊 Sajdas: " ++ show (length $ sajdas metadata) ++ " prostrations (expected 15)"
      putStrLn ""
      putStrLn "Sample data:"
      putStrLn "------------"

      -- Show first Juz
      case take 1 (juzs metadata) of
        [j] -> putStrLn $ "  First Juz: #" ++ show (juzIndex j) ++
                         " starts at " ++ show (juzStartSurah j) ++
                         ":" ++ show (juzStartAya j)
        _ -> putStrLn "  No juzs found"

      -- Show first Ruku (most important!)
      case take 1 (rukus metadata) of
        [r] -> putStrLn $ "  First Ruku: #" ++ show (rukuIndex r) ++
                         " (Surah " ++ show (rukuSurah r) ++
                         ":" ++ show (rukuStartAya r) ++ ") ⭐ THEMATIC SECTION"
        _ -> putStrLn "  No rukus found"

      -- Show last Ruku
      case reverse (rukus metadata) of
        (r:_) -> putStrLn $ "  Last Ruku: #" ++ show (rukuIndex r) ++
                          " (Surah " ++ show (rukuSurah r) ++
                          ":" ++ show (rukuStartAya r) ++ ")"
        _ -> return ()

      -- Show first and last Sajda
      case take 1 (sajdas metadata) of
        [s] -> putStrLn $ "  First Sajda: #" ++ show (sajdaIndex s) ++
                         " at " ++ show (sajdaSurah s) ++
                         ":" ++ show (sajdaAya s) ++
                         " (" ++ show (sajdaType s) ++ ")"
        _ -> putStrLn "  No sajdas found"

      case reverse (sajdas metadata) of
        (s:_) -> putStrLn $ "  Last Sajda: #" ++ show (sajdaIndex s) ++
                          " at " ++ show (sajdaSurah s) ++
                          ":" ++ show (sajdaAya s) ++
                          " (" ++ show (sajdaType s) ++ ")"
        _ -> return ()

      putStrLn ""
      putStrLn "✅ All metadata parsed successfully!"
      putStrLn "   Ready for database loading and API integration."

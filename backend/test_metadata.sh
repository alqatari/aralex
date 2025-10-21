#!/bin/bash
cabal repl aralex-backend <<REPL
import Parser.QuranMetadata
result <- parseQuranMetadataFile "../data/quran_text/quran-data.xml"
case result of { Left err -> putStrLn $ "Error: " ++ err; Right metadata -> do { putStrLn $ "Juz: " ++ show (length $ juzs metadata); putStrLn $ "Quarters: " ++ show (length $ quarters metadata); putStrLn $ "Manzils: " ++ show (length $ manzils metadata); putStrLn $ "Rukus: " ++ show (length $ rukus metadata); putStrLn $ "Pages: " ++ show (length $ pages metadata); putStrLn $ "Sajdas: " ++ show (length $ sajdas metadata) } }
:quit
REPL

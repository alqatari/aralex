import Parser.QuranMetadata
import System.Environment (getArgs)

main :: IO ()
main = do
  let xmlPath = "../data/quran_text/quran-data.xml"
  putStrLn $ "Parsing " ++ xmlPath ++ "..."
  result <- parseQuranMetadataFile xmlPath
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right metadata -> do
      putStrLn $ "✓ Successfully parsed quran-data.xml"
      putStrLn $ "  - Juz: " ++ show (length $ juzs metadata) ++ " parts"
      putStrLn $ "  - Quarters: " ++ show (length $ quarters metadata) ++ " divisions"
      putStrLn $ "  - Manzils: " ++ show (length $ manzils metadata) ++ " stations"
      putStrLn $ "  - Rukus: " ++ show (length $ rukus metadata) ++ " sections"
      putStrLn $ "  - Pages: " ++ show (length $ pages metadata) ++ " pages"
      putStrLn $ "  - Sajdas: " ++ show (length $ sajdas metadata) ++ " prostrations"
      putStrLn "\nFirst Ruku:"
      case take 1 (rukus metadata) of
        [r] -> putStrLn $ "  Ruku #" ++ show (rukuIndex r) ++ 
                         " starts at Surah " ++ show (rukuSurah r) ++ 
                         ":" ++ show (rukuStartAya r)
        _ -> putStrLn "  No rukus found"
      putStrLn "\nFirst Sajda:"
      case take 1 (sajdas metadata) of
        [s] -> putStrLn $ "  Sajda #" ++ show (sajdaIndex s) ++ 
                         " at Surah " ++ show (sajdaSurah s) ++ 
                         ":" ++ show (sajdaAya s) ++ 
                         " (" ++ show (sajdaType s) ++ ")"
        _ -> putStrLn "  No sajdas found"

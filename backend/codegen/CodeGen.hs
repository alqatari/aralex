{-|
Module      : Main (CodeGen)
Description : Generate Purescript types from Haskell domain models
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Executable to generate Purescript type definitions from Haskell types.
Run this after modifying domain types to keep frontend types in sync.
-}

module Main (main) where

import System.FilePath ((</>))
import Bridge (generateTypes)

main :: IO ()
main = do
  let outputDir = ".." </> "frontend" </> "src" </> "Generated"
  putStrLn "=== Aralex Type Generator ==="
  putStrLn $ "Generating Purescript types to: " <> outputDir
  generateTypes outputDir
  putStrLn "✅ Type generation complete!"
  putStrLn ""
  putStrLn "Next steps:"
  putStrLn "  1. cd ../frontend"
  putStrLn "  2. spago build"
  putStrLn "  3. Verify generated types in src/Generated/"

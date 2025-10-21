# Aralex Backend

Haskell backend for the Quranic Phonosemantic Analysis System.

## Building

```bash
# Install dependencies
cabal update
cabal build

# Run the server
cabal run aralex-backend

# Run tests
cabal test

# Development with auto-reload
cabal repl
```

## Architecture

- **Domain types** (`src/Domain/`) - Core domain models with GADTs and type families
- **Parsers** (`src/Parser/`) - Aeson/Attoparsec parsers for JSON/TSV data
- **Database** (`src/Database/`) - SQLite schema and data loaders
- **API** (`src/API/`) - Servant endpoints and handlers

## Data Sources

Loads data from `../data/`:
- `arabic_dicts/` - 7 classical dictionaries (40,645 entries)
- `quran_corpus/quran-morphology.txt` - Morphology corpus (130,030 segments)
- `quran_text/quran-uthmani-min.txt` - Quranic text (Uthmani script)

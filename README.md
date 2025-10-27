# Aralex - Quranic Phonosemantic Analysis System

Classical Arabic dictionary analysis combined with Quranic morphological corpus to explore letter-level phonosemantics based on Hassan Abbas's theory.

## Architecture

- **Backend**: Haskell/Servant API
- **Frontend**: PureScript/Halogen
- **Data**: 30,310 dictionary entries (deduplicated) + 130,030 Quranic morphemes
- **Type Bridge**: purescript-bridge for type-safe communication


## Data Sources

- **6 Classical Arabic Dictionaries** (30,310 entries, deduplicated)
  - Kitab al-Ain (786 CE) - 2,707 entries
  - Al-Sihah (1003 CE) - 5,594 entries
  - Maqayis al-Lugha (1004 CE) - 4,794 entries
  - Al-Muhkam (1066 CE) - 6,584 entries
  - Al-Mufradat (1108 CE) - 1,602 entries
  - Lisan al-Arab (1311 CE) - 9,029 entries

  _Note: Qamus al-Muhit (1414 CE) JSON backup retained but excluded from database to reduce size/cost._

- **Quranic Morphological Corpus** (130,030 morpheme segments)
  - Full morphological analysis with roots, lemmas, POS tags
  - UTF-8 Arabic script (no Buckwalter encoding)


## Key Features

### Backend
- Type-safe domain models with GADTs
- SQLite database with 40K+ entries
- Servant REST API
- Phonosemantic analysis engine

### Frontend
- Halogen component architecture
- Type-safe API calls (auto-generated from backend)
- Arabic text support (RTL, proper fonts)
- Interactive letter breakdown visualizations

### Type Safety
- Shared types between Haskell and PureScript
- Automatic generation via `purescript-bridge`
- Compile-time verification of API contracts

## Commands

### Backend

```bash
cd backend

# Build
cabal build

# Run server
cabal run aralex-backend

# Generate PureScript types
cabal run aralex-codegen

# Run tests
cabal test

# Hot reload dev server
./dev.sh
```

### Frontend

```bash
cd frontend

# Install dependencies
npm install

# Build
npx spago build

# Hot reload dev server
./dev.sh
```

## Requirements

- **GHC** 9.12.2+
- **Cabal** 3.12+
- **ghcid** (for hot reload)
- **Node.js** 18+
- **PureScript** (via npm)

## License

MIT

## Author

Ali Al-Qatari

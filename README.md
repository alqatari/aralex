# Aralex - Quranic Phonosemantic Analysis System

Classical Arabic dictionary analysis combined with Quranic morphological corpus to explore letter-level phonosemantics based on Hassan Abbas's theory.

## Architecture

- **Backend**: Haskell/Servant API
- **Frontend**: PureScript/Halogen
- **Data**: 40,645 dictionary entries + 130,030 Quranic morphemes
- **Type Bridge**: purescript-bridge for type-safe communication


## Data Sources

- **7 Classical Arabic Dictionaries** (40,645 entries)
  - Kitab al-Ain (786 CE)
  - Al-Sihah (1003 CE)
  - Maqayis al-Lugha (1004 CE)
  - Al-Muhkam (1066 CE)
  - Al-Mufradat (1108 CE)
  - Lisan al-Arab (1311 CE)
  - Qamus al-Muhit (1414 CE)

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

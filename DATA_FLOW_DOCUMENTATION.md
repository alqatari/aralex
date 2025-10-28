# Aralex Data Flow Documentation

## Data Sources (in `data/` directory)

### 1. Dictionary Data → `aradicts.db`
**Source Files:** `data/arabic_dicts/*.json` (6 dictionaries)
- `ain_entries_*.json` - Kitab al-Ain
- `sihah_entries_*.json` - Al-Sihah  
- `maqayis_entries_*.json` - Maqayis al-Lugha
- `al_muhkam_entries_*.json` - Al-Muhkam
- `mufradat_complete_shamela.json` - Al-Mufradat
- `lisan_entries_*.json` - Lisan al-Arab

**Database:** `backend/aradicts.db`
**Tables:**
- `dictionary_sources` - Metadata about 6 dictionaries
- `dictionary_entries` - 30,310 root definitions

**API Endpoint:** `GET /api/v1/dictionary/:root`

### 2. Quranic Morphology Data → `aralex.db`
**Source File:** `data/quran_corpus/quran-morphology.txt` (6 MB, 130,030 lines)
- **Format:** TSV with columns: `location | surface | POS | features`
- **Location:** `surah:verse:word:segment` (e.g., `5:89:15:1`)
- **Features:** Pipe-delimited (e.g., `ROOT:سكن|LEM:مِسْكِين|MP|GEN`)
- **Critical:** Contains **ROOT** field extracted from corpus

**Database:** `backend/aralex.db`
**Table:** `quranic_words`
```sql
CREATE TABLE quranic_words (
  id INTEGER PRIMARY KEY,
  surah INTEGER NOT NULL,
  verse INTEGER NOT NULL,
  word_position INTEGER NOT NULL,
  surface_form TEXT NOT NULL,
  root TEXT,                    -- ← EXTRACTED FROM CORPUS ROOT: field
  lemma TEXT,
  pos TEXT NOT NULL,
  segments_json TEXT NOT NULL,
  UNIQUE(surah, verse, word_position)
);
```

**API Endpoint:** `GET /api/v1/morphology/:surah/:verse`
**Returns:** `[QuranicWordDTO]` where each word includes:
```json
{
  "dtoWordRoot": "يمن",        // ← ROOT from corpus
  "dtoFullSurface": "أَيْمَٰنِكُمْ",
  "dtoWordPOS": "Noun",
  "dtoSegments": [...]
}
```

### 3. Quranic Text Data → `aralex.db`
**Source File:** `data/quran_text/quran-simple.txt` (pipe-delimited)
- **Format:** `surah|verse|text`
- **Script:** Uthmani with full diacritics

**Database:** `backend/aralex.db`
**Table:** `verses`

### 4. Quranic Metadata → `aralex.db`
**Source File:** `data/quran_text/quran-data.xml`
- Sura names, Juz, Quarters, Manzils, **Rukus**, Pages, Sajdas

**Database:** Multiple tables (juz, quarters, manzils, rukus, pages, sajdas)

### 5. Letter Meanings → `aralex.db`
**Source File:** `data/letter_meanings/arabic_letters.json`
- Hassan Abbas phonosemantic theory

**Database:** `aralex.db`
**Table:** `letter_meanings`

## Root Highlighting Data Flow

### Current (CORRECT) Flow:
```
data/quran_corpus/quran-morphology.txt
  → Line: "5:89:15:1  مَسَٰكِينَ  N  ROOT:سكن|LEM:مِسْكِين|MP|GEN"
    → Parser extracts ROOT:سكن
      → Database.Loader stores in aralex.db.quranic_words.root
        → API.Handlers.lookupMorphology queries database
          → Returns QuranicWordDTO with dtoWordRoot = Just "سكن"
            → Frontend receives word.dtoWordRoot = Just "سكن"
              → Frontend compares: word.dtoWordRoot == searchedRoot
                → ✅ EXACT MATCH (no pattern matching!)
```

### Example: Verse 5:89
```
Word 6:  أَيْمَٰنِكُمْ  → ROOT:يمن  ✅ Matches search "يمن" → RED
Word 11: الْأَيْمَٰنَ  → ROOT:يمن  ✅ Matches search "يمن" → RED
Word 15: مَسَٰكِينَ   → ROOT:سكن  ❌ Does NOT match "يمن" → BLACK
```

## Key Insight

**ROOT information comes from the Quranic corpus (`quran-morphology.txt`), NOT from pattern matching!**

The corpus was linguistically analyzed by scholars who manually tagged each segment with its root. This is authoritative data that should be used directly via `word.dtoWordRoot`, not approximated with text-matching algorithms.

## Frontend Root Matching (Fixed)

**BEFORE (Wrong):**
```purescript
wordMatchesRoot = wordContainsRoot (removeTashkeel word.dtoFullSurface) searchRoot
-- Pattern matching on surface form - causes false positives!
```

**AFTER (Correct):**
```purescript
wordMatchesRoot = case word.dtoWordRoot of
  Just root -> removeTashkeel root == removeTashkeel searchRoot
  Nothing -> false
-- Direct comparison of database roots - accurate!
```

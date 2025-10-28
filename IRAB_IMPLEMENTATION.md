# I'rab (إعراب) Implementation Guide

**Last Updated:** 2025-10-28  
**Status:** Phase 1 Complete (Word-level), Phase 2 In Progress (Segment-level)

---

## Table of Contents

1. [Overview](#overview)
2. [Reference: Quranic Corpus Methodology](#reference-quranic-corpus-methodology)
3. [Current Implementation (Phase 1)](#current-implementation-phase-1)
4. [Database Schema](#database-schema)
5. [Data Generation Pipeline](#data-generation-pipeline)
6. [Limitations and Known Issues](#limitations-and-known-issues)
7. [Future Work (Phase 2)](#future-work-phase-2)
8. [Code References](#code-references)

---

## Overview

Traditional Arabic grammatical analysis (إعراب) provides detailed information about each word's grammatical function, case marking, and syntactic role in Quranic verses.

**Key Principle:** Always reference the [Quranic Corpus](https://corpus.quran.com/) as the authoritative standard for i'rab methodology.

### Coverage Statistics

- **Total Quran Words:** 77,429
- **Verses:** 6,236 (100%)
- **I'rab Entries:** 77,429 (100% coverage)
- **Declinable Words (with case marking):** 22,001 (28.4%)
- **Indeclinable Words:** 55,428 (71.6%)

---

## Reference: Quranic Corpus Methodology

**IMPORTANT:** The [Quranic Corpus](https://corpus.quran.com/) is the PRIMARY REFERENCE for all i'rab decisions.

### How Quranic Corpus Handles I'rab

The corpus uses **segment-level analysis** where each morphological segment receives its own grammatical annotation:

#### Example 1: Vocative Construction (5:15:1)

Word: **يَٰٓأَهْلَ** (O People)

```
Segment 1: يَٰٓ
  - Type: VOC (vocative particle)
  - Arabic: أداة نداء
  - Analysis: حرف نداء مبني على السكون لا محل له من الإعراب

Segment 2: أَهْلَ
  - Type: N (accusative noun)
  - Arabic: اسم منصوب
  - Analysis: منادى منصوب وعلامة نصبه الفتحة
```

#### Example 2: Preposition + Noun (2:2:7)

Word: **لِّلْمُتَّقِينَ** (for the righteous)

```
Segment 1: لِّ
  - Type: P (preposition lām)
  - Arabic: حرف جر
  
Segment 2: لْ
  - Type: DET (determiner ال)
  - Arabic: أل التعريف

Segment 3: مُتَّقِينَ
  - Type: N (genitive masculine plural active participle)
  - Arabic: اسم مجرور
```

### Key Takeaways from Corpus

1. **Segment-level granularity** - Each prefix, stem, and suffix analyzed separately
2. **Explicit case identification** - Even when case is implicit in word form
3. **Contextual role assignment** - Role determined by position and surrounding words
4. **Traditional terminology** - Uses classical Arabic grammatical terms

---

## Current Implementation (Phase 1)

### Approach: Word-Level with Special Handling

**Status:** ✅ Complete and deployed

Current implementation generates i'rab at the **word level** with special handling for common multi-segment constructions.

### What Works

1. **100% Coverage** - All 77,429 words have i'rab entries
2. **Declinable Words** - Full case analysis for 22,001 words with explicit case marking (CaseDTO)
3. **Vocative Constructions** - Proper two-part analysis for "يا + noun" (189 verses)
4. **Basic Particle Recognition** - Identifies prepositions, conjunctions, negation particles

### Generation Method

**Source:** Rule-based inference from Quranic Corpus morphology data (NOT LLM-generated)

```python
# Confidence scoring
base_confidence = 0.7
+ 0.1 if has_root
+ 0.1 if role_inferred
+ 0.1 if many_features (>3)
= 0.7 - 1.0 confidence score
```

### I'rab Components

Each i'rab entry contains:

- **grammatical_role** (الدور النحوي): مبتدأ، خبر، فاعل، مفعول به، منادى، etc.
- **construction_type** (نوع التركيب): اسم، فعل، حرف، اسم فاعل، etc.
- **case_type** (الحالة): معرب (declinable) or مبني (indeclinable)
- **case_marker** (علامة الإعراب): بالضمة، بالفتحة، بالكسرة, على السكون
- **case_position** (المحل الإعرابي): في محل رفع، في محل نصب، في محل جر
- **full_irab_text** (النص الكامل): Complete traditional i'rab text in Arabic
- **source**: "inferred" (from corpus morphology)
- **confidence**: 0.7 - 1.0

---

## Database Schema

### Current Schema (Phase 1.5 - Segment-ready)

```sql
CREATE TABLE irab_details (
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Location (includes segment for future use)
  surah INTEGER NOT NULL,
  verse INTEGER NOT NULL,
  word_position INTEGER NOT NULL,
  segment_position INTEGER NOT NULL DEFAULT 1,  -- Added for Phase 2

  -- Segment identification
  segment_surface TEXT NOT NULL,  -- Surface form of segment

  -- Parsed i'rab components
  grammatical_role TEXT,
  construction_type TEXT,
  case_type TEXT,
  case_marker TEXT,
  case_position TEXT,

  -- Full text
  full_irab_text TEXT NOT NULL,

  -- Metadata
  source TEXT NOT NULL,              -- 'inferred' | 'aljadwal_ocr' | 'manual'
  confidence REAL DEFAULT 0.8,
  volume_number INTEGER,             -- For الجدول source
  page_number INTEGER,               -- For الجدول source

  -- Timestamps
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  -- Constraints
  UNIQUE(surah, verse, word_position, segment_position, source)
);

-- Indexes
CREATE INDEX idx_irab_verse ON irab_details(surah, verse);
CREATE INDEX idx_irab_word ON irab_details(surah, verse, word_position);
CREATE INDEX idx_irab_role ON irab_details(grammatical_role);
CREATE INDEX idx_irab_source ON irab_details(source);
```

### Key Design Decisions

1. **segment_position added** - Schema ready for segment-level i'rab (Phase 2)
2. **segment_surface** - Stores individual segment text, not full word
3. **Multiple sources supported** - Can have inferred + manual + OCR for same word
4. **Confidence scoring** - Allows filtering by quality threshold

---

## Data Generation Pipeline

### Source Data: Quranic Morphological Corpus

**File:** `data/quran_corpus/quran-morphology.txt` (130,030 morpheme segments)

**Format:**
```
location → surface_form → POS → features
1:1:1:1  بِ              P     P|PREF|LEM:ب
1:1:1:2  سْمِ            N     ROOT:سمو|LEM:اسْم|M|GEN
```

### Generation Scripts

#### 1. Word-Level Generator (Current)

**File:** `scripts/generate_irab_from_morphology.py`

**Process:**
1. Load word with all segments from `quranic_words` table
2. Extract features from all segments (aggregated)
3. Analyze word-level POS and context
4. Infer grammatical role based on:
   - Position in verse
   - Case marking (if present in features)
   - Previous/next word context
   - Special constructions (vocative, negation, etc.)
5. Generate full i'rab text
6. Save to `irab_details` with `segment_position = 1`

**Special Handling:**

```python
# Vocative construction detection
if len(segments) >= 2:
    first_surface = segments[0]["dtoSurface"]
    if first_surface in ["يَا", "يَٰٓ", "يَٰ", "أَ", "أَيَا"]:
        second_has_case = has_case_in_features(segments[1])
        if second_has_case:
            role = "منادى"
            # Generate two-part i'rab:
            # «يَا» حرف نداء... ، «أَهْلَ» منادى منصوب...
```

#### 2. Batch Generation

**File:** `scripts/generate_complete_irab.py`

Generates i'rab for all 6,236 verses in batches with progress tracking.

#### 3. Vocative Fix Script

**File:** `scripts/fix_vocative_irab.py`

Updates 189 verses with vocative constructions to use proper two-part analysis.

#### 4. Segment-Level Generator (Phase 2 - In Progress)

**File:** `scripts/generate_segment_irab.py`

**Process:**
1. Iterate through each segment in word
2. Analyze segment individually:
   - Check if prefix (PREF tag, position 1)
   - Check if suffix (SUFF tag, pronoun)
   - Check if stem (noun/verb with case)
3. Determine segment-specific role:
   - Prefix: حرف جر، حرف عطف، أداة تعريف، etc.
   - Stem: Role based on case and context
   - Suffix: ضمير متصل
4. Generate segment-level i'rab
5. Save multiple entries per word (one per segment)

---

## Limitations and Known Issues

### Phase 1 Limitations

1. **Word-Level Analysis Only**
   - Multi-segment words analyzed as single unit
   - Prefixes/suffixes not separately annotated
   - **Impact:** Less granular than Quranic Corpus standard
   - **Workaround:** Special handling for common constructions (vocatives)

2. **Case Detection Limited to Explicit Tags**
   - Only words with `CaseDTO` feature have case analysis
   - ~28% of words (22,001) have explicit case
   - Remaining 72% (particles, pronouns, many verbs) marked as indeclinable
   - **Missing:** Heuristic case detection from word endings (ون، ين، ات, etc.)

3. **Context-Limited Role Inference**
   - Uses only immediate previous/next word
   - **Missing:** Sentence-level syntactic parsing
   - **Impact:** May misidentify فاعل vs مفعول به in complex sentences

4. **No Deep Verb Analysis**
   - Verbs marked as فعل (generic)
   - **Missing:** Detailed mood analysis for مضارع (IND/SUBJ/JUS)
   - **Missing:** Subject pronoun extraction from verb endings

5. **Compound Construction Gaps**
   - Only vocative (يا + noun) has full two-part analysis
   - **Missing:** Preposition + pronoun analysis (عليه → على + ه)
   - **Missing:** Emphatic particle + noun (إن + noun)
   - **Missing:** Conditional constructions

### Known Edge Cases

#### Case 1: Sound Plural Endings Without CaseDTO

**Example:** `مُتَّقِينَ` (2:2:7) - segment 3

**Issue:** Word ends in `ين` (genitive/accusative plural marker), but morphology lacks explicit `CaseDTO` tag.

**Current Behavior:** Marked as generic `معرب` without specific case

**Needed:** Pattern matching for plural endings:
- `ون` → Nominative sound masculine plural
- `ين` → Genitive/Accusative sound masculine plural  
- `ات` → Sound feminine plural

#### Case 2: Attached Pronouns on Verbs vs Nouns

**Example:** `جَآءَكُمْ` (came to you) vs `رَبُّكُمْ` (your Lord)

**Issue:** Pronoun `كُمْ` has different grammatical roles:
- On verb: في محل نصب مفعول به (object)
- On noun: في محل جر بالإضافة (possessive)

**Current Behavior:** Not distinguished at segment level

**Needed:** Check POS of stem segment to determine pronoun role

#### Case 3: Assimilated/Changed Letter Forms

**Example:** ال + ش → الش (sun letters assimilation)

**Issue:** Determiner `ال` changes pronunciation/form before sun letters

**Current Behavior:** Detected via `DeterminerDTO` tag regardless of form

**Status:** ✅ Works correctly

---

## Future Work (Phase 2)

### Roadmap: Segment-Level I'rab Implementation

#### Step 1: Case Inference Enhancement

**Goal:** Detect case from word endings when CaseDTO missing

**Implementation:**
```python
def infer_case_from_ending(surface: str, features: List[Dict]) -> Optional[str]:
    """Infer case from morphological patterns"""
    
    # Sound masculine plural
    if surface.endswith(("ُونَ", "ُون")):
        return "NOM"
    if surface.endswith(("ِينَ", "ِين")):
        return "GEN" or "ACC"  # Need context
    
    # Dual
    if surface.endswith(("َانِ", "َان")):
        return "NOM"
    if surface.endswith(("َيْنِ", "َيْن")):
        return "GEN" or "ACC"
    
    # Sound feminine plural
    if surface.endswith(("َاتٌ", "َاتُ")):
        return "NOM"
    if surface.endswith(("َاتٍ", "َاتِ")):
        return "GEN"
    if surface.endswith(("َاتً", "َاتَ")):
        return "ACC"
    
    # Defective nouns (ends in ي/ى)
    # ... more patterns
    
    return None
```

**Testing:** Verify against Quranic Corpus case annotations

#### Step 2: Complete Segment-Level Generation

**Goal:** Generate separate i'rab for each segment

**Tasks:**
1. ✅ Update database schema (DONE - schema ready)
2. ✅ Create segment iterator (DONE - `generate_segment_irab.py`)
3. ⚠️  Implement prefix detection and analysis (PARTIAL)
4. ⚠️  Implement suffix/pronoun analysis (PARTIAL)
5. ❌ Implement stem analysis with context
6. ❌ Handle special prefix+stem+suffix interactions
7. ❌ Test against all 130,030 segments
8. ❌ Update backend API to return segment-level i'rab
9. ❌ Update frontend to display segment-level i'rab

**Estimated Effort:** 3-5 days of focused development

#### Step 3: Syntactic Context Analysis

**Goal:** Improve role inference using sentence-level syntax

**Approach:**
- Build dependency parse tree for each verse
- Use grammatical agreement (gender/number) for validation
- Implement verb-subject-object identification
- Handle إضافة (genitive construct) chains

**Dependencies:** May require external Arabic NLP library or custom parser

#### Step 4: الجدول OCR Integration

**Goal:** Extract detailed i'rab from "الجدول في إعراب القرآن" PDFs

**Status:** PDFs available in `data/quran_corpus/`, extraction scripts started but incomplete

**Tasks:**
1. Complete PDF text extraction (`extract_aljadwal.py`)
2. Parse traditional i'rab text structure
3. Map to verse:word:segment positions
4. Store with `source = 'aljadwal_ocr'`
5. Display alongside inferred i'rab for comparison

**Estimated Effort:** 2-3 days (OCR quality dependent)

#### Step 5: Manual Review Interface

**Goal:** Allow scholars to correct/verify i'rab

**Features:**
- Edit i'rab entries in web UI
- Save with `source = 'manual'` and higher confidence
- Track reviewer identity and timestamp
- Export corrections for training future generators

---

## Code References

### Backend (Haskell)

**Domain Types:**
- `backend/src/Domain/Irab.hs` - IrabDetail and IrabSource types

**Database:**
- `backend/src/Database/Schema.hs` - Table schema definitions
- `backend/src/Database/Loader.hs` - Data loading utilities

**API:**
- `backend/src/API/Handlers.hs` - `lookupIrab :: Connection -> Int -> Int -> IO [IrabDetail]`
- `backend/src/API/Routes.hs` - `/api/v1/irab/:surah/:verse` endpoint

**Query:**
```haskell
lookupIrab :: Connection -> Int -> Int -> IO [IrabDetail]
lookupIrab conn surahNum verseNum =
  query
    conn
    "SELECT word_position, segment_surface, grammatical_role, construction_type, \
    \case_type, case_marker, case_position, full_irab_text, source, confidence, \
    \volume_number, page_number \
    \FROM irab_details \
    \WHERE surah = ? AND verse = ? \
    \ORDER BY word_position, segment_position"
    (surahNum, verseNum)
```

**Note:** Current query orders by `word_position` only. When implementing segment-level i'rab, add `segment_position` to ORDER BY.

### Frontend (PureScript)

**Types:**
- `frontend/src/Generated/Domain/Irab.purs` - Auto-generated from Haskell types

**State:**
```purescript
type State = {
  -- ...
  , verseIrab :: Map String (Array IrabDetail)  -- Cached by verse key
}
```

**Actions:**
```purescript
data Action =
  -- ...
  | FetchIrab Int Int  -- Fetch i'rab for verse
  | ReceiveIrab String (Array IrabDetail)  -- Store result
```

**Display:**
- `frontend/src/Main.purs` - `renderIrabSection` function (lines ~1465-1540)
- Source attribution displayed: "مستنتج من المدونة الصرفية للقرآن الكريم"

### Python Generation Scripts

**Main Scripts:**
- `scripts/generate_irab_from_morphology.py` - Word-level generator (Phase 1)
- `scripts/generate_complete_irab.py` - Batch generation for all verses
- `scripts/fix_vocative_irab.py` - Vocative construction fix
- `scripts/generate_segment_irab.py` - Segment-level generator (Phase 2, in progress)

**Database Operations:**
```python
class IrabGenerator:
    def generate_verse_irab(self, surah: int, verse: int) -> List[IrabInferred]:
        """Generate i'rab for all words in verse"""
        
    def save_to_database(self, surah: int, verse: int, irab_list: List[IrabInferred]):
        """Save to irab_details table"""
```

---

## Testing Against Quranic Corpus

### Validation Process

1. **Select test verses** covering various grammatical constructions
2. **Extract i'rab from corpus** manually or via scraping
3. **Compare with generated i'rab:**
   - Grammatical role matches?
   - Case marking correct?
   - Construction type accurate?
4. **Calculate accuracy metrics:**
   - Precision: % of generated i'rab that match corpus
   - Recall: % of corpus i'rab captured
   - F1 score: Harmonic mean

### Test Cases to Implement

```python
test_cases = [
    # Vocative
    (5, 15, 1, "يَٰٓأَهْلَ", ["حرف نداء", "منادى"]),
    
    # Preposition + noun
    (2, 2, 7, "لِّلْمُتَّقِينَ", ["حرف جر", "أل", "اسم مجرور"]),
    
    # Verb with pronoun subject
    (1, 5, 2, "نَعْبُدُ", ["فعل مضارع", "فاعل ضمير"]),
    
    # Negation + noun
    (2, 2, 3, "لَا", ["حرف نفي"]),
    (2, 2, 4, "رَيْبَ", ["اسم لا"]),
    
    # Add more...
]
```

---

## Best Practices

### When Adding New I'rab Logic

1. **✅ CHECK QURANIC CORPUS FIRST**
   - Always verify approach matches corpus methodology
   - Use corpus as ground truth for edge cases

2. **Document assumptions**
   - Comment why specific rule applied
   - Reference classical grammar source if available

3. **Add test cases**
   - Include verse reference
   - Show expected vs actual i'rab

4. **Update confidence scores**
   - Lower confidence for uncertain inferences
   - Higher confidence for rule-based matches

5. **Preserve source attribution**
   - Always set `source` field correctly
   - Never mix inferred and manual without distinction

### When Debugging I'rab Issues

1. **Check morphology source:**
   ```sql
   SELECT segments_json FROM quranic_words 
   WHERE surah = X AND verse = Y AND word_position = Z;
   ```

2. **Compare with corpus:**
   - Visit https://corpus.quran.com/wordbyword.jsp?chapter=X&verse=Y
   - Check segment breakdown and case marking

3. **Verify generator logic:**
   - Add print statements to trace decision path
   - Check feature extraction (CaseDTO, PrefixDTO, etc.)

4. **Test in isolation:**
   ```python
   generator = IrabGenerator()
   irab_list = generator.generate_verse_irab(surah, verse)
   for irab in irab_list:
       print(f"{irab.word_surface}: {irab.full_irab_text}")
   ```

---

## Glossary of Arabic Grammatical Terms

| English | Arabic | Description |
|---------|--------|-------------|
| Subject | فاعل | Doer of verb action (nominative) |
| Object | مفعول به | Receiver of verb action (accusative) |
| Predicate | خبر | Information about subject (nominative) |
| Topic | مبتدأ | Subject of nominal sentence (nominative) |
| Vocative | منادى | Word being addressed/called (accusative or built) |
| Genitive | مجرور / مضاف إليه | After preposition or in possessive construct |
| Declinable | معرب | Word with case endings that change |
| Indeclinable | مبني | Word with fixed form (particles, pronouns) |
| Case marker | علامة الإعراب | Physical indication of case (damma, fatha, kasra) |
| Grammatical position | محل إعرابي | Logical case position for indeclinable words |
| Vocative particle | حرف نداء | يا، أيا، أ (calling particle) |
| Preposition | حرف جر | ب، ل، في، من، etc. |
| Conjunction | حرف عطف | و، ف، ثم، etc. |
| Negation particle | حرف نفي | لا، ما، etc. |

---

## Version History

- **2025-10-28:** Phase 1 complete, segment schema added, vocative fix deployed
- **2025-10-27:** Initial word-level generation, 100% coverage achieved
- **2025-10-26:** Database schema designed, generator prototyped

---

## References

1. **Quranic Corpus:** https://corpus.quran.com/ (PRIMARY REFERENCE - ALWAYS CHECK FIRST)
2. **Quranic Arabic Corpus GitHub:** https://github.com/quran/corpus.quran.com
3. **الجدول في إعراب القرآن** - Mahmoud Safi (14 volumes, PDFs in `data/quran_corpus/`)
4. **Traditional Grammar References:**
   - ألفية ابن مالك (Alfiyyah of Ibn Malik)
   - شرح قطر الندى وبل الصدى (Ibn Hisham)
   - النحو الواضح (Al-Nahw al-Wadhih)

---

## Contact / Contribution

For questions about i'rab implementation or to contribute improvements:

- Check existing issues in repository
- Reference this document when proposing changes
- Always validate against Quranic Corpus methodology
- Include test cases with verse references

**Remember:** The Quranic Corpus is the authoritative standard. When in doubt, check the corpus website!

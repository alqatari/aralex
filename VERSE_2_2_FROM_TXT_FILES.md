# Verse 2:2 - Complete Data from TXT Files

## Source Files in `data/` Directory

### 1. From `data/quran_text/quran-simple.txt`

**Format:** `surah|verse|text`

```
2|2|ذَٰلِكَ الْكِتَابُ لَا رَيْبَ ۛ فِيهِ ۛ هُدًى لِّلْمُتَّقِينَ
```

**Fields:**
- Surah: 2
- Verse: 2
- Text: ذَٰلِكَ الْكِتَابُ لَا رَيْبَ ۛ فِيهِ ۛ هُدًى لِّلْمُتَّقِينَ (Uthmani script with full diacritics)

---

### 2. From `data/quran_corpus/quran-morphology.txt`

**Format:** `location → surface → POS → features` (TSV)

**Location format:** `surah:verse:word:segment`

#### Complete Morphological Data (13 segments for 7 words):

```
2:2:1:1	ذَٰ	N	DEM|LEM:ذا|MS
2:2:1:2	لِ	P	DIST|SUFF|LEM:ل
2:2:1:3	كَ	P	ADDR|SUFF|M
2:2:2:1	ٱلْ	P	DET|PREF|LEM:ال
2:2:2:2	كِتَٰبُ	N	ROOT:كتب|LEM:كِتاب|M|NOM
2:2:3:1	لَا	P	NEG|LEM:لا|FAM:إِنّ
2:2:4:1	رَيْبَ	N	ROOT:ريب|LEM:رَيْب|M|ACC
2:2:5:1	فِي	P	P|LEM:فِي
2:2:5:2	هِ	N	PRON|SUFF|3MS
2:2:6:1	هُدًى	N	ROOT:هدي|LEM:هُدًى|M|INDEF|NOM
2:2:7:1	لِّ	P	P|PREF|LEM:ل
2:2:7:2	لْ	P	DET|PREF|LEM:ال
2:2:7:3	مُتَّقِينَ	N	ACT_PCPL|VF:8|ROOT:وقي|LEM:مُتَّقي|MP|GEN
```

#### Word-by-Word Breakdown:

**Word 1: ذَٰلِكَ (3 segments)**
- Segment 1: `ذَٰ` (Noun) - Features: `DEM|LEM:ذا|MS`
  - DEM = Demonstrative
  - LEM:ذا = Lemma: ذا
  - MS = Masculine Singular
  
- Segment 2: `لِ` (Particle) - Features: `DIST|SUFF|LEM:ل`
  - DIST = Distance marker (far demonstrative)
  - SUFF = Suffix
  - LEM:ل = Lemma: ل
  
- Segment 3: `كَ` (Particle) - Features: `ADDR|SUFF|M`
  - ADDR = Addressee marker
  - SUFF = Suffix
  - M = Masculine

**Word 2: ٱلْكِتَٰبُ (2 segments)**
- Segment 1: `ٱلْ` (Particle) - Features: `DET|PREF|LEM:ال`
  - DET = Definite article
  - PREF = Prefix
  - LEM:ال = Lemma: ال
  
- Segment 2: `كِتَٰبُ` (Noun) - Features: `ROOT:كتب|LEM:كِتاب|M|NOM`
  - **ROOT:كتب** = Root: كتب (to write)
  - LEM:كِتاب = Lemma: كِتاب (book)
  - M = Masculine
  - NOM = Nominative case

**Word 3: لَا (1 segment)**
- Segment 1: `لَا` (Particle) - Features: `NEG|LEM:لا|FAM:إِنّ`
  - NEG = Negation particle
  - LEM:لا = Lemma: لا
  - FAM:إِنّ = Belongs to إِنّ particle family

**Word 4: رَيْبَ (1 segment)**
- Segment 1: `رَيْبَ` (Noun) - Features: `ROOT:ريب|LEM:رَيْب|M|ACC`
  - **ROOT:ريب** = Root: ريب (doubt)
  - LEM:رَيْب = Lemma: رَيْب (doubt)
  - M = Masculine
  - ACC = Accusative case

**Word 5: فِيهِ (2 segments)**
- Segment 1: `فِي` (Particle) - Features: `P|LEM:فِي`
  - P = Preposition
  - LEM:فِي = Lemma: فِي (in)
  
- Segment 2: `هِ` (Noun) - Features: `PRON|SUFF|3MS`
  - PRON = Pronoun
  - SUFF = Suffix
  - 3MS = 3rd person, masculine, singular

**Word 6: هُدًى (1 segment)**
- Segment 1: `هُدًى` (Noun) - Features: `ROOT:هدي|LEM:هُدًى|M|INDEF|NOM`
  - **ROOT:هدي** = Root: هدي (to guide)
  - LEM:هُدًى = Lemma: هُدًى (guidance)
  - M = Masculine
  - INDEF = Indefinite
  - NOM = Nominative case

**Word 7: لِّلْمُتَّقِينَ (3 segments)**
- Segment 1: `لِّ` (Particle) - Features: `P|PREF|LEM:ل`
  - P = Preposition
  - PREF = Prefix
  - LEM:ل = Lemma: ل (for)
  
- Segment 2: `لْ` (Particle) - Features: `DET|PREF|LEM:ال`
  - DET = Definite article
  - PREF = Prefix
  - LEM:ال = Lemma: ال
  
- Segment 3: `مُتَّقِينَ` (Noun) - Features: `ACT_PCPL|VF:8|ROOT:وقي|LEM:مُتَّقي|MP|GEN`
  - ACT_PCPL = Active participle
  - VF:8 = Verb Form VIII (افتعل pattern)
  - **ROOT:وقي** = Root: وقي (to protect/guard/be conscious of)
  - LEM:مُتَّقي = Lemma: مُتَّقي (one who is conscious)
  - MP = Masculine Plural
  - GEN = Genitive case

---

### 3. From `data/quran_text/quran-data.xml`

**Surah Metadata:**
```xml
<sura index="2" ayas="286" start="7" name="البقرة" tname="Al-Baqara" ename="The Cow" type="Medinan" order="87" rukus="40" />
```

**Fields:**
- index: 2 (Surah number)
- ayas: 286 (total verses in Surah)
- start: 7 (starting Juz)
- name: البقرة (Arabic name)
- tname: Al-Baqara (transliterated name)
- ename: The Cow (English name)
- type: Medinan (revelation location)
- order: 87 (chronological order of revelation)
- rukus: 40 (number of Rukus in Surah)

**Ruku Information for Verse 2:2:**
```xml
<ruku index="2" sura="2" aya="1" />
```
- Verse 2:2 belongs to **Ruku 2**, which starts at verse 2:1
- This Ruku contains the opening verses describing the Quran and believers

---

## Summary of TXT File Data

### Roots Found (4 roots):
1. **كتب** (k-t-b) - "to write" → Word 2: الْكِتَابُ
2. **ريب** (r-y-b) - "to doubt" → Word 4: رَيْبَ
3. **هدي** (h-d-y) - "to guide" → Word 6: هُدًى
4. **وقي** (w-q-y) - "to protect/guard" → Word 7: الْمُتَّقِينَ

### POS Distribution:
- **Nouns (N):** 7 segments (ذَٰ, كِتَٰبُ, رَيْبَ, هِ, هُدًى, مُتَّقِينَ)
- **Particles (P):** 6 segments (لِ, كَ, ٱلْ, لَا, فِي, لِّ, لْ)

### Case Markers:
- **NOM (Nominative):** كِتَٰبُ, هُدًى
- **ACC (Accusative):** رَيْبَ
- **GEN (Genitive):** مُتَّقِينَ

### Special Features:
- **Demonstrative:** ذَٰلِكَ (with distance + addressee markers)
- **Negation:** لَا (from إِنّ family)
- **Active Participle Form VIII:** مُتَّقِينَ (from اتَّقَى)
- **Pronouns:** هِ (3rd person masculine singular)
- **Definiteness:** ال prefix on كِتَٰبُ and مُتَّقِينَ

---

## Data Flow Summary

```
TXT Files (Source of Truth)
├── quran-simple.txt → Verse text (Uthmani script)
├── quran-morphology.txt → Complete morphological analysis (130,030 segments)
└── quran-data.xml → Metadata (Surah names, Rukus, Juz, etc.)

                ↓ Parsed by backend/src/Parser/*.hs

Database (aralex.db)
├── verses table → Text + normalized text
├── quranic_words table → Aggregated words with roots
└── metadata tables → Rukus, Juz, Quarters, etc.

                ↓ Queried by API

API Endpoints
├── GET /api/v1/morphology/2/2 → Returns QuranicWordDTO[]
└── GET /api/v1/verse/2/2 → Returns VerseText

                ↓ Consumed by frontend

Frontend Display
└── Clickable words with grammatical analysis panels
```

All grammatical information comes directly from the **Quranic Corpus TXT file** - an authoritative, scholar-reviewed morphological analysis. No algorithms, no guessing, just pure linguistic data.

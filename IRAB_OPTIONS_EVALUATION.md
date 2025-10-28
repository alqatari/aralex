# Evaluation: إعراب Data Source Options

## What You Have Downloaded

### 1. **Quranic Corpus Morphology 0.4** ✅
- **File:** `quranic-corpus-morphology-0.4.zip` (1 MB)
- **Contains:** `quranic-corpus-morphology-0.4.txt` (6.3 MB)
- **Format:** Plain text TSV
- **Status:** Already in your project! This is what you're currently using
- **Coverage:** 100% morphology (ROOT, POS, features)
- **Limitation:** NO syntax tree/dependency/full إعراب

### 2. **الجدول في إعراب القرآن** (17 volumes)
- **File:** `Eidh416_gmail_01_201801.zip` (318 MB)
- **Contains:** 17 PDF files with "_text" suffix (total 333 MB)
- **Format:** PDF with embedded text layer
- **Author:** محمود صافي (Mahmoud Safi)
- **Coverage:** 100% complete إعراب for entire Quran
- **Status:** Extracted volume 1 successfully

---

## PDF Quality Assessment

### What I Found:
✅ **Text layer exists** (not scanned images - good!)
✅ **Arabic characters extractable** (972 Arabic chars found on page 51)
❌ **RTL extraction issues** (text comes out garbled/reversed)
❌ **Formatting corruption** (spaces, line breaks mangled)

### Example from Page 51:
```
Extracted: "ةرقبلا ةروس" (reversed: "سورة البقرة")
Extracted: "ضرم ممولق يف" (reversed: "في قلومم مرض")
```

**Conclusion:** PDF has text but needs special RTL-aware extraction

---

## The Three Real Options

### **Option A: Quranic Corpus Syntax Treebank (XML)**

**What:** Download from corpus.quran.com/download/

**Pros:**
✅ Machine-readable XML format
✅ Gold-standard linguistic annotation
✅ Dependency graphs showing grammatical relationships
✅ Already aligned with morphology you have
✅ Programmatic access (parse XML in Haskell)
✅ GNU GPL license (open source)

**Cons:**
❌ **Only 50% complete** (major issue!)
❌ Requires email registration
❌ Unknown if full إعراب text included (might be just graph structure)
❌ Need to map dependency labels to traditional terms

**Coverage:** 50% of Quran
**Effort:** Low (XML parsing in Haskell)
**Quality:** High (scholarly reviewed)

---

### **Option B: Extract from الجدول PDF**

**What:** Parse the 17 PDF volumes you downloaded

**Pros:**
✅ **100% coverage** - complete Quran
✅ Traditional scholarly إعراب (محمود صافي)
✅ Full explanatory text (not just labels)
✅ Already downloaded (no registration needed)
✅ Authoritative source (widely used reference)

**Cons:**
❌ **RTL extraction challenges** (requires special handling)
❌ **Unstructured format** (narrative text, not tables)
❌ **Alignment needed** (match text to verse:word positions)
❌ **High effort** (custom parser needed)
❌ **Potential OCR errors** despite text layer

**Example of what's in the PDF:**
```
[Verse number]
[Quranic text]
[Word 1]: إعراب details...
[Word 2]: إعراب details...
[Grammatical notes]
```

**Coverage:** 100% of Quran
**Effort:** High (PDF extraction + parsing + alignment)
**Quality:** Highest (complete traditional إعراب)

---

### **Option C: Turath.io Website**

**What:** Structured web platform displaying الجدول

**Pros:**
✅ Clean formatted display
✅ Searchable interface
✅ Organized by verses
✅ May have API (unconfirmed)

**Cons:**
❌ **No public API documented** (searched extensively)
❌ Would require web scraping (legal/ethical concerns)
❌ Rate limiting issues
❌ Dependent on external service
❌ No bulk download option found

**Verdict:** Not practical for programmatic access

---

## Technical Comparison

| Criterion | Corpus XML | الجدول PDF | Turath.io |
|-----------|------------|------------|-----------|
| **Coverage** | 50% | 100% | 100% |
| **Format** | XML ⭐ | PDF ⚠️ | HTML |
| **Effort** | Low | High | Medium |
| **Quality** | High | Highest | High |
| **Programmatic** | ✅ Easy | ⚠️ Hard | ❌ Scraping |
| **Ready to use** | ✅ Yes | ❌ No | ❌ No |
| **Legal** | ✅ GPL | ✅ Archive.org | ⚠️ TOS |

---

## Extracting from الجدول PDF - Technical Approach

### Challenge: RTL Text Extraction

**The Problem:**
```python
PyPDF2.extract_text() → "ةرقبلا ةروس"  # Reversed!
Correct should be:      "سورة البقرة"
```

**Solutions:**

#### Solution 1: Use `pdfplumber` (Better RTL support)
```python
import pdfplumber

with pdfplumber.open('volume.pdf') as pdf:
    page = pdf.pages[50]
    text = page.extract_text(layout=True)  # Preserves layout
    # Still may need RTL reversal
```

#### Solution 2: Use `pdfminer.six` (Low-level control)
```python
from pdfminer.high_level import extract_text_to_fp
from pdfminer.layout import LAParams

laparams = LAParams(detect_vertical=True)
extract_text_to_fp(input_file, output_file, laparams=laparams)
```

#### Solution 3: Reverse extracted text programmatically
```python
from bidi.algorithm import get_display
import arabic_reshaper

def fix_rtl_text(text):
    reshaped = arabic_reshaper.reshape(text)
    display_text = get_display(reshaped)
    return display_text
```

### Parsing Strategy:

**Step 1:** Extract page-by-page
**Step 2:** Detect verse boundaries (regex: `\d+:\d+`)
**Step 3:** Parse إعراب blocks per word
**Step 4:** Align with verse:word positions from morphology
**Step 5:** Store in database

**Estimated effort:** 2-4 weeks development + testing

---

## My Recommendation

### **Hybrid Approach (Best of Both Worlds)**

#### Phase 1: Use Corpus XML (Immediate - 1 week)
✅ Download syntax treebank from corpus.quran.com
✅ Parse XML dependency graphs
✅ Extract grammatical role labels
✅ Store in `aralex.db` for 50% of verses
✅ Show in UI with label: "من المدونة اللغوية"

#### Phase 2: Extract الجدول PDF (2-4 weeks)
✅ Write Python script with RTL-aware extraction
✅ Parse traditional إعراب text
✅ Align with verse:word positions
✅ Fill remaining 50% + enhance existing 50%
✅ Show in UI with label: "من الجدول في إعراب القرآن"

#### Phase 3: Validation & Enhancement (1-2 weeks)
✅ Cross-validate corpus vs الجدول
✅ Resolve conflicts (trust الجدول as authoritative)
✅ Add UI toggle: "simplified" (corpus labels) vs "detailed" (الجدول full text)

---

## Immediate Next Steps

### For Corpus XML:
1. Go to https://corpus.quran.com/download/
2. Register with email
3. Download syntax treebank (XML format)
4. Unzip and inspect structure
5. Write Haskell XML parser
6. Extract dependency relationships
7. Map to Arabic grammatical terms

### For الجدول PDF:
1. Install Python RTL libraries:
   ```bash
   pip install pdfplumber python-bidi arabic-reshaper
   ```
2. Write extraction script for volume 1
3. Test on Surah Al-Baqarah (verse 2:2)
4. Verify alignment with morphology
5. Scale to all 17 volumes

---

## Why NOT Turath.io?

❌ No documented API
❌ Scraping raises legal/ethical issues
❌ Dependent on external service uptime
❌ Can't match your offline-first architecture
❌ You already have the PDF!

**Turath.io is great for humans, not for programmatic data extraction**

---

## Final Verdict

🥇 **Best Immediate Solution:** Download Corpus XML treebank (50% coverage, low effort)

🥈 **Best Long-term Solution:** Extract الجدول PDF (100% coverage, authoritative, but high effort)

🥉 **Best Strategy:** Hybrid approach combining both

**Timeline:**
- **Week 1:** Corpus XML integration (50% working)
- **Weeks 2-4:** PDF extraction development
- **Weeks 5-6:** Validation and UI enhancement
- **Result:** 100% coverage with gold-standard traditional إعراب

---

## Technical Architecture

```
Data Sources
├── Quranic Corpus XML (50%)
│   └── Dependency graphs → Grammatical roles
│
├── الجدول PDF (100%)
│   └── Traditional إعراب text → Full analysis
│
└── Your Existing Morphology (100%)
    └── ROOT, POS, case markers

            ↓ Parsers (Haskell + Python)

aralex.db (New Table)
├── irab_details
│   ├── surah, verse, word_position
│   ├── grammatical_role (مبتدأ، خبر، فاعل...)
│   ├── case_ending (الضمة الظاهرة...)
│   ├── irab_full_text (complete traditional text)
│   ├── source ('corpus_xml' | 'aljadwal_pdf')
│   └── confidence (high | medium | low)

            ↓ API

Frontend Display
└── Grammar Panel with detailed إعراب
    ├── Simple mode: role labels only
    └── Detailed mode: full traditional text
```

---

## Questions for You

Before proceeding, please decide:

1. **Should I start with Corpus XML now?** (Quick win, 50% coverage)
2. **Do you want me to develop PDF extraction?** (More complex, 100% coverage)
3. **Hybrid approach OK?** (Both sources, best quality)
4. **Priority: speed or completeness?** (XML fast, PDF complete)

The PDFs you have are **gold** - they contain the complete traditional إعراب. The challenge is extraction, not data availability.

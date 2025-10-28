# Research: Filling the إعراب (I'rab) Gap

## The Gap We Need to Fill

**What we have:** Morphological features (ROOT, POS, case markers like NOM/ACC/GEN, gender, number)

**What we need:** Full traditional إعراب like:
- Grammatical role labels: مبتدأ، خبر، فاعل، مفعول به، حال، تمييز
- Complete case analysis: "مرفوع وعلامة رفعه الضمة الظاهرة على آخره"
- Syntactic relationships: "في محل رفع خبر المبتدأ"
- Dependency structure: which word depends on which

---

## Solution 1: Quranic Arabic Corpus Syntax Treebank

### Overview
- **Website:** https://corpus.quran.com/
- **GitHub:** https://github.com/kaisdukes/quranic-corpus
- **License:** GNU General Public License (Open Source)
- **Status:** 50% complete (as of latest update)

### What It Provides
✅ **Dependency graphs** based on traditional Arabic إعراب
✅ **Syntactic treebank** with grammatical relationships
✅ **Visual grammar diagrams** showing dependencies
✅ **Traditional grammar concepts:**
  - Nominals: gender, adjectives, إضافة, apposition, تمييز
  - Verbs: forms, subjects, objects, كان وأخواتها, moods
  - Particles: إِنّ وأخواتها, حرف عطف, vocatives, exceptives
  - Adverbials: حال، مفعول مطلق، مفعول لأجله، مفعول معه
  - Clauses: شرط وجواب شرط, relative clauses

### Available Data
- **Download page:** https://corpus.quran.com/download/
- **Format:** XML (for treebank), TSV/text for morphology
- **Coverage:** 77,430 words
- **Version:** 0.4 (morphology complete, syntax 50% complete)

### Limitations
❌ **Only 50% of syntax diagrams completed** - major gap!
❌ **Unclear if full إعراب text is included** (might only be visual graphs)
❌ **May not have detailed علامات الإعراب explanations** in text form
❌ **Requires email registration to download**

### How to Access
1. Visit https://corpus.quran.com/download/
2. Register with email
3. Download XML syntax treebank + morphology data
4. Parse XML to extract dependency relationships

---

## Solution 2: CAMeL Tools (Python Library)

### Overview
- **GitHub:** https://github.com/CAMeL-Lab/camel_tools
- **Organization:** NYU Abu Dhabi - CAMeL Lab
- **License:** MIT (Open Source)
- **Latest:** Camel Morph MSA (2024) - presented at LREC-COLING 2024

### What It Provides
✅ **Morphological analysis** (100K+ lemmas, 1.45B analyses)
✅ **CamelParser** - dependency parsing with syntactic case
✅ **Diacritization** with case assignment rules
✅ **NER, Sentiment Analysis, Dialect ID**

### Capabilities
- **Dependency parsing** aligned with morphological features
- **Syntactic case analysis** (improving diacritization through syntax)
- **Agreement rules** for case and state
- **Modern Standard Arabic + Classical Arabic features**

### Limitations
❌ **NOT pre-trained on Quran specifically** (general MSA)
❌ **Generates predictions**, not gold-standard annotations
❌ **May not match traditional إعراب exactly** (computational vs. traditional)
❌ **Requires Python integration** (you're using Haskell/Purescript)
❌ **No "full sentence إعراب text"** - outputs structured features

### How to Use
```python
pip install camel-tools

from camel_tools.morphology.database import MorphologyDB
from camel_tools.disambig.mle import MLEDisambiguator
from camel_tools.parsers.camel_parser import CamelParser

# Load parser
parser = CamelParser.pretrained()

# Parse sentence
result = parser.parse(sentence)
# Returns dependency tree with morphological features
```

---

## Solution 3: Stanza (Stanford NLP)

### Overview
- **Website:** https://stanfordnlp.github.io/stanza/
- **Organization:** Stanford NLP Group
- **License:** Apache 2.0 (Open Source)
- **Languages:** 70+ including Arabic

### What It Provides
✅ **Universal Dependencies** parsing (standard format)
✅ **Tokenization, lemmatization, POS tagging**
✅ **Dependency parsing** with syntactic relations
✅ **Morphological features** (gender, case, number, etc.)

### Arabic Models
- Trained on **Universal Dependencies Arabic corpora**
- Supports **Modern Standard Arabic**
- Uses **PADT treebank** (Prague Arabic Dependency Treebank)

### Limitations
❌ **NOT Quran-specific** (general MSA/news text)
❌ **Universal Dependencies ≠ Traditional Arabic Grammar**
  - UD uses labels like `nsubj`, `obj`, `obl`
  - Traditional uses مبتدأ، خبر، فاعل، مفعول به
❌ **No full إعراب text generation**
❌ **Requires Python** (same issue as CAMeL)

### How to Use
```python
import stanza

# Download Arabic model
stanza.download('ar')
nlp = stanza.Pipeline('ar')

# Parse
doc = nlp(text)
for sentence in doc.sentences:
    print(sentence.dependencies)
    # Returns UD-style dependency tuples
```

---

## Solution 4: Traditional إعراب Books (Digitized)

### Overview
Multiple complete إعراب القرآن books are available as **digitized PDFs** on Internet Archive

### Available Books
1. **"إعراب القرآن وبيانه"** - محيي الدين الدرويش
   - URL: https://archive.org/details/5_20200604_20200604_0241
   - Complete traditional إعراب for entire Quran
   
2. **"إعراب القرآن الكريم"** - دار الصحابة للتراث
   - URL: https://archive.org/details/erabquran_sahaba
   - Scholarly traditional إعراب
   
3. **"الجدول في إعراب القرآن وصرفه وبيانه"** - محمود صافي
   - URL: https://archive.org/details/Eidh416_gmail_01_201801
   - Size: 3.4GB (comprehensive)
   - Tables format (easier to parse?)
   
4. **"إعراب القرآن الكريم"** - د. محمود سليمان ياقوت
   - URL: https://archive.org/details/Eidh416_gmail_20180122_1018

### What They Provide
✅ **Complete traditional إعراب** for every word
✅ **Full explanations** like "اسم إشارة مبني على السكون في محل رفع مبتدأ"
✅ **علامات الإعراب** details
✅ **Syntactic relationships** explained
✅ **Covers 100% of Quran** (not 50%)

### Limitations
❌ **PDF format** - requires OCR to extract
❌ **Unstructured text** - needs parsing/alignment with verses
❌ **Manual digitization work required**
❌ **No machine-readable format** (not JSON/XML)
❌ **OCR errors** (especially with Arabic diacritics)

### How to Use
1. Download PDFs from Internet Archive
2. OCR with Arabic support (Tesseract, Adobe Acrobat)
3. Parse text to extract إعراب per word
4. Align with Quranic text (verse:word matching)
5. Store in database as structured data

---

## Comparison Matrix

| Solution | Coverage | Format | Traditional إعراب | Ready to Use | Integration |
|----------|----------|--------|-------------------|--------------|-------------|
| **Quranic Corpus Treebank** | 50% | XML | Partial | ⚠️ Partial | Medium |
| **CAMeL Tools** | 100% | Computed | No (UD-style) | ✅ Yes | Hard (Python) |
| **Stanza** | 100% | Computed | No (UD-style) | ✅ Yes | Hard (Python) |
| **Traditional Books** | 100% | PDF | ✅ Full | ❌ No | Very Hard (OCR) |

---

## Recommended Approach

### Short Term (Use What Works Now):
**Option A: Corpus Treebank (50% coverage)**
- Download XML from corpus.quran.com
- Parse dependency relationships
- Map to traditional grammar terms where available
- Accept 50% coverage limitation

**Option B: Expand Existing Morphology**
- Use your current morphology data
- Write Haskell rules to infer grammatical roles from:
  - POS tags + case markers → likely role (NOM after DEM = مبتدأ)
  - Verb + NOM noun = likely فاعل
  - Verb + ACC noun = likely مفعول به
- Generate "partial إعراب" programmatically
- Less accurate but covers 100%

### Long Term (Complete Solution):
**Option C: Digitize Traditional إعراب**
1. Download "الجدول في إعراب القرآن" (table format = easier)
2. OCR with Tesseract Arabic
3. Write Haskell parser to extract structured data:
   ```
   Verse 2:2, Word 1 (ذلك):
   - Role: مبتدأ
   - Case: مرفوع
   - Reason: اسم إشارة مبني على السكون في محل رفع
   ```
4. Store in `aralex.db` new table: `irab_details`
5. Display in frontend grammar panel

**Hybrid: Corpus Treebank (50%) + Generated Rules (50%)**
- Use treebank where available (high quality)
- Generate from rules for remaining verses
- Flag which is which to user

---

## Technical Recommendation

**For Aralex specifically, I recommend:**

### Phase 1 (Immediate - 2 weeks):
✅ Download Quranic Corpus XML syntax treebank
✅ Parse dependency graphs to extract grammatical roles
✅ Map UD-style relations to Arabic terms (if needed)
✅ Add to database for 50% of verses
✅ Show in UI: "التحليل النحوي متاح لـ 50% من الآيات"

### Phase 2 (Medium term - 1-2 months):
✅ Write Haskell grammar rules for common patterns:
  - Nominal sentences (جملة اسمية)
  - Verbal sentences (جملة فعلية)
  - Particle sentences (جملة حرفية)
✅ Infer grammatical roles from morphology + context
✅ Cover remaining 50% with "inferred" label
✅ Show in UI: "محسوب آلياً" vs "من المدونة اللغوية"

### Phase 3 (Long term - 3-6 months):
✅ OCR one traditional إعراب book ("الجدول" recommended)
✅ Parse and align with verse:word positions
✅ Validate against existing data
✅ Replace inferred data with gold-standard
✅ Achieve 100% coverage with traditional إعراب

---

## Python Integration (If Chosen)

If you decide to use CAMeL Tools or Stanza:

### Architecture:
```
Haskell Backend (Port 8080)
    ↓ HTTP call
Python Microservice (Port 8081)
    ↓ Uses CAMeL/Stanza
Returns JSON with irab features
    ↓
Haskell parses and stores in DB
```

### Python Service:
```python
from flask import Flask, request, jsonify
from camel_tools.parsers.camel_parser import CamelParser

app = Flask(__name__)
parser = CamelParser.pretrained()

@app.route('/parse', methods=['POST'])
def parse_text():
    text = request.json['text']
    result = parser.parse(text)
    return jsonify(result.to_dict())

if __name__ == '__main__':
    app.run(port=8081)
```

### Haskell Client:
```haskell
import Network.HTTP.Simple

callPythonParser :: Text -> IO ParseResult
callPythonParser text = do
  let request = setRequestBodyJSON (object ["text" .= text])
              $ parseRequest_ "POST http://localhost:8081/parse"
  response <- httpJSON request
  pure $ getResponseBody response
```

---

## Final Verdict

**Best solution for Aralex:**

🥇 **Primary: Quranic Corpus XML Treebank** (50% coverage, authoritative)
🥈 **Secondary: Rule-based inference** (cover remaining 50%, marked as computed)
🥉 **Future: OCR traditional books** (100% gold-standard when resources allow)

**Avoid:**
- ❌ CAMeL/Stanza for now (adds Python dependency, not Quran-specific)
- ❌ Manual entry (too time-consuming)

**The gap CAN be filled**, but requires combining:
1. Existing corpus data (partial)
2. Programmatic rules (intermediate)
3. Traditional scholarship digitization (complete)

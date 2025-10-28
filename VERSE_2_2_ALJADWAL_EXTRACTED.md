# Verse 2:2 - Extracted from الجدول في إعراب القرآن

## Source
**PDF:** Volume 1, Page 32
**Book:** الجدول في إعراب القرآن وصرفه وبيانه
**Author:** محمود صافي

---

## Extracted Text (Raw from PDF)

### Lines 19-30 from Page 32:

```
19: لحم ‎ادمعفر
20: :بارعإلا ىلعينبمةراشإمسا)اذ( نوكسلا يف
21: )ماللا(و )فاكلارودعبلل .باطخلل )باتكلا( نملدب »)اذ( ‎فطعوأ
22: ‎حتف
23: "عفرلايفهعبتنايب سنجللةيفان)ال( )بير( لاىلعينبمالمسا
24: )بيصنفلحم(يف )ءاهلارورجفرح ‎لحميفينبملصتمريمض
25: وکنأزوجي))۱( و وچ ا
```

### Cleaned and Reconstructed:

**الإعراب:**

- **(ذا)**: اسم إشارة مبني على السكون في محل رفع مبتدأ
- **اللام**: للبعد
- **الكاف**: للخطاب
- **(الكتاب)**: بدل من (ذا) أو عطف بيان
- **(لا)**: نافية للجنس
- **(ريب)**: اسم لا مبني على الفتح في محل نصب
- **(فيه)**: (في) حرف جر، (الهاء) ضمير متصل مبني في محل جر

---

## Format Analysis

### الجدول Structure:

1. **Verse reference** (implied or numbered)
2. **Word-by-word إعراب** in format: **(word)**: إعراب details...
3. **Morphological notes** (الصرف section follows)

### Key Observations:

✅ **Complete traditional إعراب** included
✅ **Word-by-word breakdown** with parentheses ()
✅ **Grammatical roles clearly stated**: مبتدأ، اسم لا، حرف جر
✅ **Case endings explained**: مبني على السكون، مبني على الفتح
✅ **Positional analysis**: في محل رفع، في محل نصب، في محل جر

---

## Extraction Challenges

### Issues Found:
1. **Word spacing** - words run together: "ىلعينبمةراشإمسا" → "اسم إشارة مبني على"
2. **RTL reversal** - some text backwards: "رودعبلل" → "للبعد"
3. **Line breaks** - إعراب split across lines
4. **Parentheses** - used to mark words: (ذا), (الكتاب)

### Solutions Needed:
1. ✅ **Character grouping** - already working
2. ⚠️ **Better word separation** - needs improvement (gap detection)
3. ⚠️ **RTL fix** - partial (some words still reversed)
4. ✅ **Parenthesis detection** - can identify word markers

---

## Next Steps

### To Get Complete Clean Text:

1. **Improve word gap detection** - current: 8px, may need adjustment
2. **Post-process common patterns**:
   - "مبني على السكون" (mabni 'ala al-sukun)
   - "في محل رفع/نصب/جر" (fi mahall raf'/nasb/jarr)
   - "اسم إشارة" (ism ishara)
3. **Parse parenthesized words** - extract (word) as markers
4. **Reconstruct multi-line إعراب** - group lines belonging to same word

### Pattern Recognition:

The format appears to be:
```
(word): grammatical_role details case_info positional_phrase
```

Example:
```
(ذا): اسم إشارة مبني على السكون في محل رفع مبتدأ
```

This is **parseable** with regex patterns!

---

## Database Schema Implications

Based on this format, we need:

```sql
CREATE TABLE irab_details (
  id INTEGER PRIMARY KEY,
  surah INTEGER NOT NULL,
  verse INTEGER NOT NULL,
  word_position INTEGER NOT NULL,
  word_surface TEXT NOT NULL,  -- From (word) markers
  
  -- Grammatical analysis
  grammatical_role TEXT,        -- مبتدأ، خبر، فاعل, etc.
  construction_type TEXT,       -- اسم إشارة، فعل ماضي, etc.
  case_type TEXT,               -- مبني/معرب
  case_marker TEXT,             -- على السكون، على الفتح
  case_position TEXT,           -- في محل رفع/نصب/جر
  full_irab_text TEXT NOT NULL, -- Complete traditional text
  
  -- Metadata
  source TEXT DEFAULT 'aljadwal',
  page_number INTEGER,
  
  FOREIGN KEY (surah, verse, word_position) 
    REFERENCES quranic_words(surah, verse, word_position)
);
```

---

## Success Indicators

✅ Found الجدول content for verse 2:2
✅ Extracted إعراب text successfully  
✅ Identified structure and format
✅ Text is readable (with cleanup needed)
✅ Pattern is consistent and parseable

**Next:** Write parser to clean and structure this data for all verses!

# Morphology File Abbreviations & Acronyms Reference

**Source**: `data/quran_corpus/morphology-terms.json`  
**Document**: Complete decoding of all abbreviations used in `quran-morphology.txt`

---

## PART OF SPEECH (POS) TYPES

### Nouns & Pronouns
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **N** | Noun | اسم | Any common noun (book, tree, person) |
| **PN** | Proper Noun | علم | Specific names (Muhammad, Egypt, Allah) |
| **PRON** | Pronoun | ضمير | he, she, it, they, we, I, etc. |
| **DEM** | Demonstrative | اسم إشارة | this, that, these, those |
| **REL** | Relative Pronoun | اسم موصول | which, who, that (relative) |
| **T** | Temporal Adverb | ظرف زمان | when, then, today, yesterday |
| **LOC** | Locative Adverb | ظرف مكان | where, here, there, above, below |

### Verbs
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **V** | Verb | فعل | Main action word (go, write, see) |
| **NV** | Noun of Verb | اسم فعل | Special verbal noun forms |

### Other POS
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **COND** | Conditional | شرطية | if, unless (conditional particle) |
| **INTG** | Interrogative | استفهامية | question words (who, what, why) |
| **P** | Particle | حرف جر | Any particle/function word (prepositions, etc.) |

---

## PARTICLES (40+ Types)

### Basic Particles
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **P** | Particle (Preposition) | حرف جر | Indicates prepositional phrases (in, on, at, from) |
| **EMPH** | Emphatic Lam | لام التوكيد | Emphasizes preceding word |
| **IMPV** | Imperative Lam | لام الأمر | Indicates imperative command |
| **PRP** | Purpose/Reason Lam | لام التعليل | Shows reason or purpose |

### Conjunction & Coordination
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **CONJ** | Conjunction | حرف عطف | Connects words/phrases (and, or, but) |
| **COM** | Companionship Waw | واو المعية | "with" or "and" showing accompaniment |
| **EQ** | Equalization | حرف تسوية | Shows equality (whether...or) |

### Causative & Reason
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **SUB** | Substantive | حرف مصدري | Introduces verbal noun phrases |
| **CAUS** | Causative | حرف سببية | Shows cause/reason |
| **ACC** | Accusative Marker | حرف نصب | Marks accusative case on nouns |

### Negation & Exceptions
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **NEG** | Negation | حرف نفي | Negates meaning (not, no, never) |
| **EXP** | Exception Particle | أداة استثناء | Marks exceptions (except, but) |
| **RES** | Restriction/Focus | أداة حصر | Restricts focus (only, just) |
| **PRO** | Prohibition | حرف نهي | Marks negative command (don't) |

### Emphatic & Modifying Particles
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **CERT** | Certainty | حرف تحقيق | Emphasizes certainty |
| **EXH** | Exhortation | حرف تحضيض | Encourages action |
| **EXL** | Explanation | حرف تفصيل | Provides detailed explanation |
| **INT** | Interpretation | حرف تفسير | Clarifies or explains preceding |
| **ATT** | Attention | حرف تنبيه | Draws attention (verily, behold) |

### Discourse & Connection
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **AMD** | Retraction | حرف استدراك | Corrects/retracts previous statement |
| **ANS** | Response | حرف جواب | Marks response clause |
| **AVR** | Aversion | حرف ردع | Marks disapproval |
| **CIRC** | Circumstantial | حرف حال | Introduces circumstantial clause |
| **REM** | Resumption | حرف استئنافية | Resumes previous topic |
| **RET** | Retraction | حرف اضراب | Marks retraction/shift in discourse |
| **RSLT** | Result in Condition | حرف واقع في جواب الشرط | Marks result clause of conditional |
| **FUT** | Future | حرف استقبال | Marks future/prospective |
| **INC** | Inceptive | حرف ابتداء | Marks beginning of new statement |

### Special Particles
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **VOC** | Vocative | حرف نداء | Calls/addresses someone (O, hey) |
| **SUR** | Surprise | حرف فجاءة | Marks sudden occurrence |
| **SUP** | Supernumerary | حرف زائد | Decorative/extra particle (grammatically not needed) |
| **PREV** | Prepositive Kaf | حرف كاف | Special prepositive "kaf" (like, as) |
| **DIST** | Distant Lam | لام البعد | Shows distance in time/place |
| **ADDR** | Address | حرف خطاب | Marks direct address |
| **INL** | Isolated Letters | حروف مقطعة | Quranic isolated letters (Alif-Lam-Meem, etc.) |

---

## NOUN FORMS & GRAMMAR

### Noun Forms
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **ACT_PCPL** | Active Participle | اسم فاعل | One who does action (doer) |
| **PASS_PCPL** | Passive Participle | اسم مفعول | One who receives action (done-to) |
| **VN** | Verbal Noun | مصدر | Abstract noun from verb |

### Grammatical Cases
| Code | English Name | Arabic Name | Grammatical Function |
|------|--------------|-------------|----------------------|
| **NOM** | Nominative | مرفوع | Subject position (raised case) |
| **ACC** | Accusative | منصوب | Object/complement position (lowered case) |
| **GEN** | Genitive | مجرور | Possession/prepositional position |

### Noun Attributes
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **ADJ** | Adjective | نعت | Descriptive modifier |
| **INDEF** | Indefinite | نكرة | Without "the" (a book, not the book) |
| **DEF** | Definite | معرّف | With "the" (marked by ال) |
| **PASS** | Passive Voice | لم يسمّ فاعله | Agent is not named |

---

## VERB FORMS & GRAMMAR

### Verb Tenses
| Code | English Name | Arabic Name | Time Reference |
|------|--------------|-------------|-----------------|
| **PERF** | Perfect | ماض | Past/completed action |
| **IMPF** | Imperfect | مضارع | Present/ongoing/habitual action |
| **IMPV** | Imperative | أمر | Command (do this!) |

### Verb Moods (for Imperfect)
| Code | English Name | Arabic Name | Grammatical Effect |
|------|--------------|-------------|-------------------|
| **IND** | Indicative | مرفوع | Neutral mood (raised ending) |
| **SUBJ** | Subjunctive | منصوب | After certain particles (lowered ending) |
| **JUS** | Jussive | مجزوم | After negatives/conditionals (clipped ending) |

### Verb Forms (Derivational)
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **VF:1** | Form I (Basic) | الفعل الثلاثي البسيط | Simple triliteral (eat, drink, go) |
| **VF:2** | Form II (Doubled) | فَعَّلَ | Intensified action (break → shatter) |
| **VF:3** | Form III (Sociative) | فاعَلَ | Reciprocal/mutual action |
| **VF:4** | Form IV (Causative) | أَفْعَلَ | Causative (make someone do) |
| **VF:5** | Form V | تَفَعَّلَ | Passive of Form II |
| **VF:6** | Form VI | تَفاعَلَ | Passive of Form III |
| **VF:7** | Form VII (Passive) | انْفَعَلَ | Passive voice form |
| **VF:8** | Form VIII (Reflexive) | افْتَعَلَ | Reflexive/middle voice |
| **VF:9** | Form IX (Color) | افْعَلَّ | Qualities (color, defect) |
| **VF:10** | Form X (Factitive) | اسْتَفْعَلَ | Ask for/request action |
| **VF:11** | Form XI | افْعالَّ | Rare variant forms |

### Verb Gender & Number Markers
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **1P** | 1st Person | متكلم | I, we (speaker) |
| **2MP** | 2nd Masc Plural | مخاطب جمع مذكر | You (plural, masculine) |
| **2FS** | 2nd Fem Singular | مخاطب مؤنث | You (singular, feminine) |
| **3MS** | 3rd Masc Singular | غائب مذكر | He, it (singular, masculine) |
| **3MP** | 3rd Masc Plural | غائب جمع مذكر | They (plural, masculine) |
| **3FS** | 3rd Fem Singular | غائب مؤنث | She, it (singular, feminine) |
| **3FP** | 3rd Fem Plural | غائب جمع مؤنث | They (plural, feminine) |
| **M** | Masculine | مذكر | Grammatical gender (male) |
| **F** | Feminine | مؤنث | Grammatical gender (female) |
| **S** | Singular | مفرد | One item |
| **D** | Dual | مثنى | Exactly two |
| **P** | Plural | جمع | More than two |

---

## STRUCTURAL MARKERS & LABELS

### Basic Markers
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **ROOT** | Root | الجذر | The 3-4 letter root |
| **LEM** | Lemma | الصيغة | Dictionary form/canonical form |
| **MOOD** | Mood/Case | الإعراب | Grammatical case/mood marker |
| **VF** | Verb Form | باب الفعل | Which of the 10 Arabic verb forms |

### Morphological Components
| Code | English Name | Arabic Name | Meaning |
|------|--------------|-------------|---------|
| **PREF** | Prefix | بادئة | Attached at beginning |
| **SUFF** | Suffix | لاحقة | Attached at end |
| **DET** | Determiner | ال | The definite article (al-) |
| **PREV** | Prepositive | حرف | Pre-attached particle |
| **REM** | Resumptive | استئنافي | Resumes previous topic |

### Family Markers
| Code | English Name | Arabic Name | Function |
|------|--------------|-------------|----------|
| **FAM** | Family | حروف ناسخة | Particle family (changes meaning) |

---

## COMPLETE EXAMPLE DECODING

Let's decode one line from your image:

```
2MP|ACC|صادِق:LEM|صَدَقَ:N  ACT_PCPL|VF:1|ROOT  صادِقين  2:23:20:1
```

**Breaking it down:**

| Component | Meaning |
|-----------|---------|
| `2MP` | **2nd person, Masculine, Plural** ("you all" - masculine) |
| `ACC` | **Accusative case** (object position) |
| `صادِق:LEM` | **Lemma is صادِق** (the ones who testify - base form) |
| `صَدَقَ:N` | **Noun derived from صَدَقَ root** |
| `ACT_PCPL` | **Active Participle** (doers of the action) |
| `VF:1` | **Verb Form I** (basic form) |
| `ROOT` | Root marker (indicates صدق is the root) |
| `صادِقين` | **Surface form** (as it appears in Quran) |
| `2:23:20:1` | **Location**: Surah 2, Verse 23, Word 20, Segment 1 |

**Translation**: "testifiers" or "the ones who testify" - in accusative case, addressing multiple masculine people

---

## QUICK REFERENCE BY CATEGORY

### Pronouns & Persons
```
1, 2, 3        = Person (I, you, he/she)
M, F           = Gender (masculine, feminine)
S, D, P        = Number (singular, dual, plural)
PRON           = Pronoun word type
SUFF/PREF      = Suffix/Prefix attachments
```

### Verbs
```
V              = Verb word type
PERF/IMPF/IMPV = Tense (past, present, command)
VF:1-11        = Verb form (10 Arabic forms + rare)
IND/SUBJ/JUS   = Mood (for imperfect verbs)
1P/2MP/3MS etc = Person, gender, number
```

### Nouns
```
N              = Common noun
PN             = Proper noun
NOM/ACC/GEN    = Case (subject/object/possession)
INDEF/DEF      = With or without "the"
ACT_PCPL       = Active participle (doer)
PASS_PCPL      = Passive participle (done-to)
M, F           = Gender
S, D, P        = Number
```

### Particles & Structure
```
P              = Particle/preposition
40+ types      = Specific particle functions
ROOT           = Shows root word
LEM            = Dictionary form
PREF/SUFF      = Prefix/suffix markers
DET            = The article (ال)
```

---

## SPECIAL NOTES

### Understanding "LEM"
**LEM** stands for **Lemma** - the dictionary/canonical form of a word.

Example:
- Surface form: **كاتبين** (writers - plural accusative)
- LEM: **كاتب** (the singular base form)
- ROOT: **كتب** (the 3-letter root "write")

### Understanding Segments
The morphology file breaks words into **segments** (not whole words):

```
Word: كِتَابُهُم (their book)
├─ Segment 1: كِتَاب (book) - N ROOT:ktb LEM:كتاب
└─ Segment 2: هُم (their) - PRON|SUFF|3MP (their = suffix on noun)
```

### Verb Forms Explained
The 10 Arabic verb forms derive from a base root (3 letters):

```
Root: k-t-b (write)
VF:1: كَتَبَ   = wrote (simple past)
VF:2: كَتَّبَ  = made write/dictated
VF:3: كاتَبَ  = corresponded with
VF:4: أَكْتَبَ = caused to write
VF:8: اكْتَتَبَ = copied/wrote down
... (10 forms total)
```

### Case System (NOM/ACC/GEN)
Arabic nouns change endings based on grammatical function:

```
Root: د-ر-س (study/lesson)
NOM: دَرْس   = the lesson (subject)
ACC: دَرْس   = the lesson (object)
GEN: دَرْس   = the lesson (after preposition/possession)
```

---

## SUMMARY TABLE

| Category | Example Codes | Meaning |
|----------|---------------|---------|
| **Word Types** | N, V, P, PN, PRON | What is this word? |
| **Tense** | PERF, IMPF, IMPV | When? Past/present/command? |
| **Mood** | IND, SUBJ, JUS | How is verb affected? |
| **Person** | 1, 2, 3 | Who? (I/you/he-she) |
| **Gender** | M, F | Male or female? |
| **Number** | S, D, P | Singular/dual/plural? |
| **Case** | NOM, ACC, GEN | Grammatical position? |
| **Particles** | 40+ codes | Which particle type? |
| **Structure** | ROOT, LEM, PREF, SUFF | What's the root? Dictionary form? |

---

## REFERENCES

- **Source File**: `data/quran_corpus/morphology-terms.json`
- **Morphology Data**: `data/quran_corpus/quran-morphology.txt` (130,030 segments)
- **Total Abbreviations**: 50+ codes for morphological annotation
- **Language**: Quranic Arabic (Uthmani orthography)
- **Grammar System**: Traditional Arabic grammar (إعراب)


# Acquiring Missing Corpus.quran.com Data Components

**Document**: How to obtain and integrate syntactic treebank, semantic ontology, dictionary definitions, named entity tags, and pronoun coreference data from corpus.quran.com

**Author**: Research Team  
**Date**: 2025-10-27  
**Status**: Implementation Guide

---

## Executive Summary

We currently have **morphological data only** (130,030 segments). The Quranic Arabic Corpus at corpus.quran.com provides **five additional advanced components** that we can acquire to significantly enhance Aralex:

| Component | Status | Source | Format | Size | Priority |
|-----------|--------|--------|--------|------|----------|
| Syntactic Treebank | 🟢 Available | Website + Download | XML/TXT | ~10 MB | ⭐⭐⭐ High |
| Semantic Ontology | 🟢 Available | Download | Unknown | ~5 MB | ⭐⭐⭐ High |
| Dictionary | 🟢 Available | Download | Unknown | ~3 MB | ⭐⭐ Medium |
| Named Entity Tags | 🟡 Partial | Website + Java API | Embedded | N/A | ⭐⭐ Medium |
| Pronoun Coreference | 🟡 Partial | Java API | Embedded | N/A | ⭐ Low |

---

## 1. SYNTACTIC TREEBANK (Dependency Graphs)

### What It Is
- **Complete dependency parsing** of all 6,236 Quranic verses
- **Dependency relations** between all words in each verse
- **Syntactic structure** showing verb-argument relationships
- **Traditional Arabic grammar** annotation (إعراب format)

### Availability & Access

#### Method 1: Web Download (RECOMMENDED)
**Location**: https://corpus.quran.com/download/

**Process**:
1. Go to https://corpus.quran.com/download/
2. Enter your email address for verification
3. Download version 0.4+ of the corpus
4. Extract "Syntactic Treebank" component

**File Format**: XML or TXT
- **XML Format**: Structured dependency tree representation
- **TXT Format**: CoNLL or similar tabular format with head-dependent relations

**Expected Structure**:
```xml
<verse chapter="1" verse="1">
  <word id="1" form="بِ" lemma="ب" pos="P">
    <head id="2"/>  <!-- Points to word 2 as head -->
    <deprel>PREP</deprel>  <!-- Preposition relation -->
  </word>
  <word id="2" form="اسْمِ" lemma="اسم" pos="N">
    <head id="0"/>  <!-- Head of clause -->
    <deprel>ROOT</deprel>
  </word>
  <!-- More words... -->
</verse>
```

#### Method 2: Java API Access
**Library**: JQuranTree (available at corpus.quran.com/java/)

```java
// Pseudocode - actual API methods may differ
QuranCorpus corpus = new QuranCorpus();
Verse verse = corpus.getVerse(1, 1);
List<Word> words = verse.getWords();
for (Word w : words) {
    Word head = w.getSyntacticHead();
    String relation = w.getDependencyRelation();
}
```

#### Method 3: Research Papers & GitHub
- **Paper**: "A Dependency Treebank of the Quran using traditional Arabic grammar" (IEEE, ResearchGate)
- **Authors**: Kais Dukes, Buckwalter
- **Format**: Can extract from academic publications

### Data Structure

Each verse contains:
- **Word ID**: Position in verse (1-indexed)
- **Word Form**: Surface form with diacritics
- **Lemma**: Dictionary form
- **POS Tag**: Part of speech
- **Head ID**: ID of syntactic head (0 = sentence root)
- **Dependency Relation**: Type of grammatical relation

**Common Dependency Relations**:
- `ROOT` - Root of the clause
- `NSUBJ` - Nominal subject
- `PREP` - Preposition
- `POBJ` - Prepositional object
- `DOBJ` - Direct object
- `XOBJ` - Indirect/Extended object
- `COORD` - Coordinate
- `AUX` - Auxiliary
- `CONJ` - Conjunction
- `ADJMOD` - Adjectival modifier
- `ADVMOD` - Adverbial modifier

### Integration with Aralex

**Current Gap**: We display morphology but NOT syntax

**Enhancement Opportunities**:
1. **Dependency Graph Visualization**: Render SVG/Canvas force-directed graphs like our etymology graphs
2. **Grammatical Relationship Display**: Show verb-argument structure for each verse
3. **Clause Identification**: Identify main clauses, subordinate clauses, etc.
4. **Case Frame Analysis**: Extract predicate-argument frames for verbs
5. **Parallel Structure Detection**: Find parallel syntactic structures in the Quran

**Implementation Steps**:
```
1. Download treebank data from corpus.quran.com/download/
2. Parse XML/TXT into dependency tuples
3. Store in database:
   - quran_dependencies table (verse_id, word_id, head_id, relation)
4. Extend API:
   - GET /api/v1/dependencies/{surah}/{verse}
5. Create PureScript component:
   - DependencyGraph.purs (render as force-directed graph)
6. Update UI:
   - Add syntax tab alongside morphology tab
```

---

## 2. SEMANTIC ONTOLOGY (Concept Relationships)

### What It Is
- **300 linked Quranic concepts** (Allah, Prophet, Faith, Evil, etc.)
- **350 semantic relations** connecting concepts
- **Knowledge representation** using predicate logic
- **Grounded in tradition**: Ibn Kathir's tafsir, hadith literature

### Availability & Access

#### Method 1: Web Download
**Location**: https://corpus.quran.com/download/

**Process**:
1. Go to corpus.quran.com/download/
2. Enter email for verification
3. Download "Ontology of Concepts" component

**File Format**: Unknown (likely XML or JSON with RDF-like structure)

**Expected Structure**:
```json
{
  "concepts": [
    {
      "id": 1,
      "name": "الله",
      "english": "Allah",
      "definition": "The Almighty Creator",
      "type": "divine_being"
    },
    {
      "id": 2,
      "name": "النبي محمد",
      "english": "Prophet Muhammad",
      "definition": "The Final Prophet",
      "type": "person"
    }
  ],
  "relations": [
    {
      "source": 2,
      "target": 1,
      "type": "worships",
      "label": "يعبد"
    },
    {
      "source": 2,
      "target": 1,
      "type": "messenger_of",
      "label": "رسول"
    }
  ]
}
```

#### Method 2: Java API
```java
// Pseudocode
QuranOntology ontology = new QuranOntology();
Concept allah = ontology.getConcept("Allah");
List<Concept> relatedConcepts = allah.getRelatedConcepts();
for (Concept c : relatedConcepts) {
    String relationship = allah.getRelationTo(c);
}
```

#### Method 3: Manual Extraction from Website
Visit https://corpus.quran.com/ontology.jsp
- Browse concept categories
- Map relationships visually
- Extract via browser developer tools

### Data Structure

**Concept Properties**:
- `id` - Unique identifier
- `name_ar` - Arabic name
- `name_en` - English translation
- `definition` - Description
- `category` - Type (person, place, thing, abstract, etc.)
- `quranic_verses` - Verses mentioning this concept

**Relationship Types**:
- **Instance**: "Satan is a jinn"
- **Part-of**: "Mercy is an attribute of Allah"
- **Causes**: "Disbelief causes punishment"
- **Opposite**: "Belief is opposite to disbelief"
- **Similar**: "Forgiveness is similar to mercy"

**Example Ontology Subset**:
```
Allah (concept)
├── Attributes
│   ├── Merciful (رحمن)
│   ├── Just (عادل)
│   └── Almighty (قدير)
├── Actions
│   ├── Creates
│   ├── Forgives
│   └── Punishes
└── Related to
    ├── Prophet Muhammad
    ├── Angels
    └── Believers
```

### Integration with Aralex

**Current Gap**: We don't have semantic concept linking

**Enhancement Opportunities**:
1. **Concept Linking**: Mark words that reference ontology concepts
2. **Concept Network Graph**: Visualize concept relationships (like etymology graph)
3. **Semantic Context**: Show related concepts when analyzing a word
4. **Theme Analysis**: Identify thematic clusters in verses
5. **Commentary Integration**: Link to Ibn Kathir's tafsir concepts

**Implementation Steps**:
```
1. Download ontology from corpus.quran.com/download/
2. Parse into concept database:
   - concepts table (id, name_ar, name_en, definition, category)
   - concept_relations table (source_id, target_id, relation_type)
3. Extend morphology processor:
   - For each word, identify related concepts
   - Store word-to-concept mappings
4. Create API endpoint:
   - GET /api/v1/concepts/{word_or_root}
5. Create PureScript components:
   - ConceptNetwork.purs (graph visualization)
   - ConceptPanel.purs (concept details)
6. Update analysis display:
   - Show linked concepts below word analysis
```

---

## 3. QURANIC DICTIONARY

### What It Is
- **Lexical definitions** for Quranic vocabulary
- **Root-based organization**
- **Semantic field groupings**
- **Usage frequency** data
- **Cross-references** to other dictionary entries

### Availability & Access

#### Method 1: Web Download
**Location**: https://corpus.quran.com/download/

**Process**:
1. Go to corpus.quran.com/download/
2. Enter email
3. Download "Quran Dictionary" component

**File Format**: Likely XML, JSON, or plain text with tabular structure

**Expected Structure**:
```json
{
  "entries": [
    {
      "id": "kmtb",
      "root": "كتب",
      "lemma": "كتاب",
      "definition": "A written document or script",
      "usage_count": 245,
      "verses": [
        {"surah": 2, "verse": 2},
        {"surah": 2, "verse": 87}
      ],
      "semantic_field": "communication",
      "related_roots": ["ktb", "wrk"]
    }
  ]
}
```

#### Method 2: Web Interface Scraping
- Visit https://corpus.quran.com/qurandict.jsp
- Browse by root or alphabet
- Extract definitions and usage data

#### Method 3: Integration with Morphology Data
- Use lemma field from morphology file
- Cross-reference with our 6 classical dictionaries
- Supplement missing entries from corpus

### Data Structure

**Dictionary Entry Fields**:
- `root` - Three-letter root
- `lemma` - Dictionary form
- `definition_ar` - Arabic definition
- `definition_en` - English definition
- `pos` - Part of speech
- `frequency` - Occurrence count in Quran
- `verses` - List of verse references
- `semantic_field` - Conceptual category
- `related_roots` - Similar roots

**Semantic Fields**:
- Actions/Verbs: movement, speech, thought
- Objects/Nouns: creation, artifacts, people
- Qualities: moral, physical, temporal
- Relations: family, authority, exchange

### Integration with Aralex

**Current State**: We have 6 classical dictionaries (Ain, Sihah, Maqayis, Muhkam, Mufradat, Lisan)

**Enhancement Opportunities**:
1. **Quranic Frequency Data**: Show which dictionary entries have highest Quranic usage
2. **Semantic Field Tagging**: Categorize dictionary entries by semantic domain
3. **Cross-Dictionary Comparison**: Compare definitions across our 6 + corpus dictionary
4. **Modern Arabic Link**: Show how classical roots evolve to modern usage
5. **Frequency-Based Relevance**: Rank dictionary results by Quranic frequency

**Implementation Steps**:
```
1. Download dictionary from corpus.quran.com/download/
2. Parse and deduplicate against our 6 classical dicts
3. Extend dictionary schema:
   - Add quranic_frequency column
   - Add semantic_field column
4. Merge with existing dictionary entries
5. Update lookup API:
   - GET /api/v1/dictionary/{root}?include_frequency=true
6. Update UI to show frequency and semantic field
```

---

## 4. NAMED ENTITY TAGS (Proper Noun Classification)

### What It Is
- **Proper noun identification** (people, places, events)
- **Entity classification** (person, location, divine, etc.)
- **Quranic entity linking** (specific people, specific places)
- **Named entity resolution** (alternative names for same entity)

### Availability & Access

#### Method 1: Website Features (Embedded)
Visit verses and click on proper nouns:
- https://corpus.quran.com/quran/2/216
- Click on entity names (Muhammad, Egypt, Allah, etc.)
- Website shows entity type and related information

**Limitation**: Data not easily downloadable; requires scraping

#### Method 2: Java API
```java
// Pseudocode
QuranNER ner = new QuranNER();
Verse verse = corpus.getVerse(1, 1);
for (Word word : verse.getWords()) {
    Entity entity = ner.getEntity(word);
    if (entity != null) {
        String type = entity.getType();  // person, place, divine, etc.
        String linkedConcept = entity.getOntologyLink();
    }
}
```

#### Method 3: Manual Annotation
- Extract from research papers on NER for Quranic Arabic
- Create annotation guidelines
- Build dataset from website data

### Data Structure

**Entity Types**:
- **Person**: Muhammad, Abraham, Moses, Adam, Eve
- **Place**: Mecca, Egypt, Jerusalem, Arabia
- **Divine**: Allah, Angels, Jinn, Satan
- **Time**: Night of Decree, Day of Judgment
- **Event**: Exodus, Creation, Resurrection
- **Object**: Kaaba, Torah, Quran, Throne

**Entity Record**:
```json
{
  "id": "entity_1",
  "surface_form": "محمد",
  "lemma": "محمد",
  "type": "person",
  "canonical_name": "Muhammad",
  "arabic_name": "محمد",
  "aliases": ["النبي", "الرسول", "خاتم النبيين"],
  "ontology_link": 2,  // Links to concept_id in ontology
  "verses": ["3:144", "4:79", "5:41"],
  "description": "Final Prophet and Messenger of Allah"
}
```

### Integration with Aralex

**Current Gap**: No entity recognition or linking

**Enhancement Opportunities**:
1. **Entity Highlighting**: Highlight all named entities in verses
2. **Entity Profiles**: Show information about people, places, events
3. **Entity Networks**: Show relationships between entities
4. **Canonical Linking**: Link all mentions of same entity
5. **Frequency Analysis**: Show which entities appear most frequently

**Implementation Steps**:
```
1. Extract NER data from corpus website or API
2. Create named_entities table:
   - id, surface_form, type, canonical_name, ontology_link
3. Create entity_mentions table:
   - word_id, entity_id, surah, verse
4. Build API:
   - GET /api/v1/entities/{type}
   - GET /api/v1/entities/{name}
5. Update word analysis:
   - Show entity type when hovering over named entities
6. Create entity browser:
   - PureScript component to explore all entities
   - Show all verses mentioning each entity
```

---

## 5. PRONOUN COREFERENCE (Anaphora Resolution)

### What It Is
- **Pronoun linking** (which pronoun refers to which noun)
- **Implicit reference resolution** (ellipsis, dropped pronouns)
- **Discourse cohesion** analysis
- **Subject tracking** through verse sequences

### Availability & Access

#### Method 1: Java API (Most Likely)
```java
// Pseudocode
QuranCoreferenceResolver resolver = new QuranCoreferenceResolver();
Verse verse = corpus.getVerse(4, 3);
for (Word word : verse.getWords()) {
    if (word.isPronoun()) {
        Word antecedent = resolver.getAntecedent(word);
        if (antecedent != null) {
            System.out.println(word.getForm() + " refers to " + antecedent.getForm());
        }
    }
}
```

#### Method 2: Website Embedded Data
- Visit verses with pronouns: https://corpus.quran.com/quran/2/216
- Hover over/click pronouns to see antecedents
- Data is in HTML/JavaScript (requires extraction)

#### Method 3: Research Papers & Corpora
- Academic papers on Arabic anaphora resolution
- PropBank-like annotations for Arabic
- BERT/transformers fine-tuned on Quranic Arabic

### Data Structure

**Coreference Record**:
```json
{
  "verse_id": "2:216",
  "pronoun_word_id": 5,
  "pronoun_form": "هم",
  "antecedent_word_id": 2,
  "antecedent_form": "الذين",
  "antecedent_type": "noun",
  "confidence": 0.95,
  "reference_chain": [2, 5, 8, 12]  // All mentions of same entity
}
```

**Reference Types**:
- **Direct Pronoun**: ه, ها, هم, هن (explicit pronoun)
- **Demonstrative**: هذا, ذلك, تلك (demonstrative)
- **Relative**: الذي, التي, اللذان (relative pronoun)
- **Ellipsis**: Dropped pronouns (implicit)
- **Generic Reference**: Generic "you", "one"

### Integration with Aralex

**Current Gap**: No coreference resolution

**Enhancement Opportunities**:
1. **Anaphora Display**: Show antecedents when hovering over pronouns
2. **Reference Chains**: Display all mentions of same entity in verse
3. **Discourse Visualization**: Show pronoun-antecedent links in dependency graph
4. **Coherence Analysis**: Analyze discourse structure across verses
5. **Implicit Arguments**: Identify and mark ellipsis

**Implementation Steps**:
```
1. Extract coreference data from Java API
2. Create coreference table:
   - pronoun_id, antecedent_id, confidence, reference_chain
3. Build API:
   - GET /api/v1/coreference/{surah}/{verse}
   - GET /api/v1/reference-chains/{entity_id}
4. Update morphology display:
   - Highlight pronouns with antecedent links
   - Show reference chains on click
5. Create PureScript component:
   - CorefDisplay.purs (show linked pronouns/antecedents)
   - ReferenceChain.purs (visualize entity tracking)
```

---

## IMPLEMENTATION PRIORITIES

### Phase 1 (High Priority): Syntactic Treebank
**Why**: 
- Complementary to our phonosemantic analysis
- Enhances grammar teaching
- Supports computational analysis
- Medium implementation complexity
- Clear data format (XML/CoNLL)

**Effort**: 2-3 weeks
**Impact**: Significant (adds grammatical dimension)

### Phase 2 (High Priority): Semantic Ontology
**Why**:
- Links to our etymology system
- Supports thematic analysis
- Enables concept-based search
- Integrates with classical scholarship
- 300 concepts manageable size

**Effort**: 3-4 weeks
**Impact**: Very high (conceptual dimension)

### Phase 3 (Medium Priority): Dictionary + NER
**Why**:
- Enhances our dictionary features
- Improves entity recognition
- Supports educational features
- Medium complexity

**Effort**: 2-3 weeks each
**Impact**: Medium (enhances existing features)

### Phase 4 (Low Priority): Pronoun Coreference
**Why**:
- Specialized feature (linguists mainly)
- More complex implementation
- Requires robust disambiguation
- Lower user demand

**Effort**: 2-4 weeks
**Impact**: Low-medium (specialized use case)

---

## TECHNICAL IMPLEMENTATION ROADMAP

### Step 1: Download and Parse Data
```bash
# Download from corpus.quran.com/download/
# Register with email, receive download link

# Extract components
tar -xzf quranic-corpus-v0.4.tar.gz

# Key files to extract:
# - quranic-treebank-v0.4.xml (or .txt)
# - quranic-ontology-v0.4.json (or .xml)
# - quranic-dictionary-v0.4.json (or .txt)
# - named-entities.xml
# - coreference-links.txt
```

### Step 2: Parse and Load into Database
```haskell
-- Backend: Parse XML/JSON
module Domain.Treebank where

data DependencyRelation = ROOT | NSUBJ | DOBJ | ...
data SyntacticHead = SyntacticHead
  { headWordId :: Int
  , dependencyRelation :: DependencyRelation
  }

-- SQL Schema
CREATE TABLE syntactic_heads (
  word_id INTEGER PRIMARY KEY,
  surah INTEGER,
  verse INTEGER,
  head_word_id INTEGER,
  relation TEXT
);
```

### Step 3: Extend API Endpoints
```haskell
-- New API routes
type API = ... :<|> "syntactic-analysis" :> Capture "surah" Int :> Capture "verse" Int :> Get '[JSON] [SyntacticWord]
         :<|> "concepts" :> Capture "word" Text :> Get '[JSON] [Concept]
         :<|> "entities" :> Capture "type" Text :> Get '[JSON] [NamedEntity]
```

### Step 4: Create PureScript Components
```purescript
-- New UI components
module Component.SyntacticGraph where
-- Render dependency graphs

module Component.ConceptNetwork where
-- Render concept relationships

module Component.EntityBrowser where
-- Browse named entities
```

### Step 5: Integrate into Verse Analysis
```purescript
-- Update existing analysis panel
type AnalysisPanel = 
  { morphology :: MorphologyPanel
  , phonosemantics :: PhonosemanticPanel
  , syntax :: SyntacticPanel          -- NEW
  , concepts :: ConceptPanel          -- NEW
  , entities :: EntityPanel           -- NEW
  }
```

---

## DATA ACQUISITION CHECKLIST

### Prerequisites
- [ ] Email account (for downloading from corpus.quran.com)
- [ ] Storage space (20-30 MB for all data)
- [ ] XML/JSON parser libraries
- [ ] Database migration tools

### Action Items
- [ ] Visit https://corpus.quran.com/download/
- [ ] Register with email
- [ ] Download Treebank data
- [ ] Download Ontology data
- [ ] Download Dictionary data
- [ ] Extract and inspect file formats
- [ ] Create parsing scripts
- [ ] Load into test database
- [ ] Validate data integrity
- [ ] Document schema mappings

### Integration Steps
- [ ] Design database schema extensions
- [ ] Implement Haskell parsers
- [ ] Add API endpoints
- [ ] Create PureScript components
- [ ] Test with sample verses
- [ ] Deploy to staging environment
- [ ] Write documentation
- [ ] Deploy to production

---

## POTENTIAL CHALLENGES & SOLUTIONS

| Challenge | Solution |
|-----------|----------|
| Data format unclear | Contact corpus.quran.com team; check GitHub issues |
| File sizes large | Stream parse data; use incremental loading |
| Encoding issues | Ensure UTF-8 normalization; test with diacritics |
| Complex data structures | Use typed parsers (Aeson, Attoparsec) |
| Performance (large joins) | Create proper indexes; use materialized views |
| API rate limits | Cache treebank data; batch requests |
| Breaking changes in future versions | Version pinning; maintain backward compatibility |

---

## SUCCESS METRICS

After implementation, measure:

1. **Data Coverage**: % of verses with syntactic analysis
2. **Query Performance**: Response time for syntax/concept/entity queries
3. **User Engagement**: Click-through on syntax/concepts/entities
4. **Accuracy**: Compare annotations with academic sources
5. **Completeness**: All 6,236 verses covered

---

## References

1. **Quranic Arabic Corpus**: https://corpus.quran.com
2. **Download Page**: https://corpus.quran.com/download/
3. **Java API**: https://corpus.quran.com/java/
4. **Research Paper**: "A Dependency Treebank of the Quran using traditional Arabic grammar" (Kais Dukes, Mark Dickerson)
5. **GitHub**: https://github.com/bnjasim/quranic-corpus
6. **CoNLL Format**: https://universaldependencies.org/format.html
7. **RDF/Ontology**: https://www.w3.org/RDF/


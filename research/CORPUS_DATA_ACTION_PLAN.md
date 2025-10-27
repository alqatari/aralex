# Action Plan: Acquiring & Integrating Missing Corpus.quran.com Data

**Created**: 2025-10-27  
**Target**: Enhance Aralex with 5 advanced linguistic components  
**Timeline**: 8-12 weeks for full implementation

---

## Quick Reference

### What We Need to Get

| Component | Source | Access Method | Priority | Est. Time |
|-----------|--------|----------------|----------|-----------|
| **Syntactic Treebank** | corpus.quran.com | Download (XML) | ⭐⭐⭐ | 2-3 weeks |
| **Semantic Ontology** | corpus.quran.com | Download (JSON/XML) | ⭐⭐⭐ | 3-4 weeks |
| **Dictionary Data** | corpus.quran.com | Download | ⭐⭐ | 2-3 weeks |
| **Named Entity Tags** | corpus.quran.com + API | Website + Java API | ⭐⭐ | 2-3 weeks |
| **Pronoun Coreference** | corpus.quran.com API | Java API | ⭐ | 2-4 weeks |

---

## IMMEDIATE ACTIONS (This Week)

### 1. Register with corpus.quran.com
```
Go to: https://corpus.quran.com/download/
Action: Enter email address to request data access
Expected: Receive download link within 24-48 hours
```

### 2. Download All Available Data
```
Expected files:
- quranic-corpus-morphology-v0.4 (we already have)
- quranic-treebank-v0.4.xml or .txt (syntactic data)
- quranic-ontology-v0.4.json or .xml (semantic concepts)
- quranic-dictionary-v0.4.txt or .json (word definitions)
- named-entities.xml (person, place, entity tags)
- coreference.txt (pronoun linking)

Storage: Create data/corpus_advanced/ directory
```

### 3. Inspect File Formats
```bash
# For each downloaded file:
head -50 file.ext              # Check format
file file.ext                  # Identify file type
wc -l file.ext                 # Count lines/entries
du -h file.ext                 # Check size

# Document findings in FORMATS_DISCOVERED.md
```

### 4. Create Parser Sketches
```haskell
-- In backend/src/Parser/
module Parser.TreebankXML where
-- Parse treebank XML

module Parser.OntologyJSON where
-- Parse ontology JSON

module Parser.DictionaryText where
-- Parse dictionary format
```

---

## PHASE 1: SYNTACTIC TREEBANK (Weeks 1-3)

### Objectives
- Parse dependency data from treebank files
- Load into `syntactic_heads` database table
- Create API endpoints for syntax queries
- Render dependency graphs in PureScript

### Detailed Steps

#### 1a. Create Database Schema
```sql
-- Backend setup
CREATE TABLE syntactic_heads (
  id INTEGER PRIMARY KEY,
  word_id INTEGER NOT NULL,
  surah INTEGER NOT NULL,
  verse INTEGER NOT NULL,
  word_position INTEGER NOT NULL,
  head_word_id INTEGER,  -- NULL if root
  relation TEXT NOT NULL,  -- ROOT, NSUBJ, DOBJ, etc.
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (word_id) REFERENCES quranic_words(id),
  UNIQUE(surah, verse, word_position)
);

CREATE INDEX idx_treebank_verse ON syntactic_heads(surah, verse);
CREATE INDEX idx_treebank_word ON syntactic_heads(word_id);
```

#### 1b. Write Haskell Parser
```haskell
-- backend/src/Parser/Treebank.hs

module Parser.Treebank
  ( parseDependencyLine
  , SyntacticAnnotation(..)
  ) where

import Data.Text (Text)

data SyntacticAnnotation = SyntacticAnnotation
  { saWordId :: Int
  , saSurah :: Int
  , saVerse :: Int
  , saPosition :: Int
  , saHeadWordId :: Maybe Int
  , saRelation :: Text
  } deriving (Show, Eq)

-- Example: Parse CoNLL format
-- ID FORM LEMMA UPOS XPOS FEATS HEAD DEPREL DEPS MISC
parseDependencyLine :: Text -> Either String SyntacticAnnotation
parseDependencyLine line = do
  let fields = T.split (== '\t') line
  -- Extract fields
  -- Validate and return SyntacticAnnotation
  pure SyntacticAnnotation { ... }
```

#### 1c. Create Database Loader
```haskell
-- backend/src/Database/Loaders/Treebank.hs

module Database.Loaders.Treebank where

import Database.SQLite.Simple (Connection)

loadTreebankFile :: FilePath -> Connection -> IO ()
loadTreebankFile filePath conn = do
  contents <- readFile filePath
  let annotations = map parseDependencyLine (lines contents)
  insertAnnotations conn annotations

insertAnnotations :: Connection -> [SyntacticAnnotation] -> IO ()
insertAnnotations conn anns = do
  execute_ conn "BEGIN TRANSACTION"
  forM_ anns $ \ann -> do
    execute conn
      "INSERT INTO syntactic_heads (word_id, surah, verse, word_position, head_word_id, relation) \
       \VALUES (?, ?, ?, ?, ?, ?)"
      (saWordId ann, saSurah ann, saVerse ann, saPosition ann, saHeadWordId ann, saRelation ann)
  execute_ conn "COMMIT"
```

#### 1d. Create API Endpoint
```haskell
-- In API/Routes.hs, add:

type API = ... :<|> "syntactic-structure" :> Capture "surah" Int :> Capture "verse" Int :> Get '[JSON] [SyntacticWord]

-- In API/Handlers.hs, add:

getSyntacticStructure :: Int -> Int -> Handler [SyntacticWord]
getSyntacticStructure surah verse = liftIO $ do
  heads <- query conn
    "SELECT w.word_id, w.surface_form, sh.head_word_id, sh.relation \
     \FROM syntactic_heads sh \
     \JOIN quranic_words w ON sh.word_id = w.id \
     \WHERE sh.surah = ? AND sh.verse = ? \
     \ORDER BY w.word_position"
    (surah, verse)
  pure $ map rowToSyntacticWord heads
```

#### 1e. Create PureScript Components
```purescript
-- frontend/src/Component/SyntacticGraph.purs

module Component.SyntacticGraph where

import Halogen as H
import Halogen.HTML as HH

type State = 
  { verse :: Verse
  , dependencies :: Array SyntacticHead
  , selectedWord :: Maybe Int
  }

component :: H.Component HH.HTML Query Input Output Aff
component = H.mkComponent
  { initialState: \verse -> { verse, dependencies: [], selectedWord: Nothing }
  , render: renderGraph
  , eval: H.mkEval $ H.defaultEval
  }

renderGraph :: State -> HH.HTML Action
renderGraph state = 
  HH.div [ HP.class_ (ClassName "syntactic-graph") ]
    [ HH.canvas 
        [ HP.id_ "dependency-canvas"
        , HP.width 800
        , HP.height 600
        ]
    , HH.div [ HP.class_ (ClassName "relations-legend") ]
        [ HH.p_ [ HH.text "Dependency Relations" ]
        , renderRelationLegend
        ]
    ]
```

#### 1f. Test & Validate
```bash
# Test with known verse
# Check that all words have correct head links
# Verify no orphaned dependencies

# Unit tests:
Test.Haskell: parseDependencyLine
Test.Haskell: insertAnnotations
Test.PureScript: renderSyntacticGraph
```

---

## PHASE 2: SEMANTIC ONTOLOGY (Weeks 4-7)

### Objectives
- Parse ontology JSON/XML with 300+ concepts
- Load concepts and relationships into database
- Link words to concepts
- Create concept network visualization

### Detailed Steps

#### 2a. Database Schema
```sql
CREATE TABLE concepts (
  id INTEGER PRIMARY KEY,
  name_ar TEXT NOT NULL,
  name_en TEXT NOT NULL,
  definition_ar TEXT,
  definition_en TEXT,
  category TEXT NOT NULL,  -- person, place, divine, abstract, etc.
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE concept_relations (
  id INTEGER PRIMARY KEY,
  source_id INTEGER NOT NULL,
  target_id INTEGER NOT NULL,
  relation_type TEXT NOT NULL,  -- instance, part_of, causes, opposite, etc.
  label_ar TEXT,
  FOREIGN KEY (source_id) REFERENCES concepts(id),
  FOREIGN KEY (target_id) REFERENCES concepts(id)
);

CREATE TABLE word_concept_links (
  id INTEGER PRIMARY KEY,
  word_id INTEGER NOT NULL,
  concept_id INTEGER NOT NULL,
  surah INTEGER,
  verse INTEGER,
  confidence REAL DEFAULT 1.0,
  FOREIGN KEY (word_id) REFERENCES quranic_words(id),
  FOREIGN KEY (concept_id) REFERENCES concepts(id)
);
```

#### 2b. Haskell Types & Parser
```haskell
module Domain.Ontology where

newtype ConceptId = ConceptId { unConceptId :: Int }

data Concept = Concept
  { conceptId :: ConceptId
  , nameAr :: Text
  , nameEn :: Text
  , definitionAr :: Maybe Text
  , definitionEn :: Maybe Text
  , category :: ConceptCategory
  } deriving (Eq, Show, Generic)

data ConceptCategory = Person | Place | Divine | Abstract | Event | Object
  deriving (Eq, Show, Enum, Bounded)

data ConceptRelation = ConceptRelation
  { source :: ConceptId
  , target :: ConceptId
  , relationType :: RelationType
  , labelAr :: Maybe Text
  } deriving (Eq, Show)

data RelationType = Instance | PartOf | Causes | Opposite | Similar | Attribute
  deriving (Eq, Show, Enum)

-- Parse JSON using Aeson
instance FromJSON Concept where
  parseJSON = withObject "Concept" $ \v -> Concept
    <$> v .: "id"
    <*> v .: "name_ar"
    <*> v .: "name_en"
    <*> v .:? "definition_ar"
    <*> v .:? "definition_en"
    <*> v .: "category"
```

#### 2c. Word-Concept Linking
```haskell
-- After loading ontology, link words to concepts

module Database.Loaders.Ontology where

-- Strategy: For each word, check if its lemma matches concept name
linkWordsToConceptsautomatically :: Connection -> IO ()
linkWordsToConceptsautomatically conn = do
  words <- query_ conn "SELECT id, lemma FROM quranic_words"
  concepts <- query_ conn "SELECT id, name_ar FROM concepts"
  
  forM_ words $ \(wordId, lemma) -> do
    -- Find exact or fuzzy matches
    let matchingConcepts = filter (isSimilar lemma . snd) concepts
    forM_ matchingConcepts $ \(conceptId, _) -> do
      execute conn
        "INSERT OR IGNORE INTO word_concept_links (word_id, concept_id, confidence) VALUES (?, ?, ?)"
        (wordId, conceptId, 1.0 :: Float)

isSimilar :: Text -> Text -> Bool
isSimilar a b = normalizeArabic a == normalizeArabic b
```

#### 2d. API Endpoints
```haskell
-- Add to API:

type API = ... 
  :<|> "concepts" :> Capture "word" Text :> Get '[JSON] [Concept]
  :<|> "concept" :> Capture "id" Int :> Get '[JSON] ConceptWithRelations
  :<|> "concept-network" :> Capture "id" Int :> Get '[JSON] ConceptNetwork

-- Handlers:

getConceptsForWord :: Text -> Handler [Concept]
getConceptsForWord word = liftIO $ do
  query conn
    "SELECT c.* FROM concepts c \
     \JOIN word_concept_links wcl ON c.id = wcl.concept_id \
     \JOIN quranic_words w ON wcl.word_id = w.id \
     \WHERE w.lemma = ? OR w.surface_form = ?"
    (word, word)

getConceptWithRelations :: Int -> Handler ConceptWithRelations
getConceptWithRelations conceptId = liftIO $ do
  concept <- fetchConcept conceptId
  relations <- query conn
    "SELECT * FROM concept_relations WHERE source_id = ? OR target_id = ?"
    (conceptId, conceptId)
  pure ConceptWithRelations { concept, relations }
```

#### 2e. PureScript Components
```purescript
-- frontend/src/Component/ConceptNetwork.purs

module Component.ConceptNetwork where

type ConceptNode =
  { id :: Int
  , name :: String
  , category :: String
  , label :: String
  }

type ConceptEdge =
  { source :: Int
  , target :: Int
  , relation :: String
  , label :: String
  }

renderConceptNetwork :: Array ConceptNode -> Array ConceptEdge -> HH.HTML Action
renderConceptNetwork nodes edges =
  HH.div [ HP.class_ (ClassName "concept-network") ]
    [ HH.canvas [ HP.id_ "concept-canvas" ]
    , HH.ul [ HP.class_ (ClassName "concept-list") ]
        (map renderConceptItem nodes)
    ]
```

---

## PHASE 3: DICTIONARY + NAMED ENTITIES (Weeks 8-10)

### 3a. Extend Dictionary Schema
```sql
CREATE TABLE dictionary_entries_extended (
  dict_entry_id INTEGER PRIMARY KEY,
  quranic_frequency INTEGER DEFAULT 0,
  semantic_field TEXT,
  first_occurrence_surah INTEGER,
  first_occurrence_verse INTEGER,
  FOREIGN KEY (dict_entry_id) REFERENCES dictionary_entries(id)
);
```

### 3b. Named Entities
```sql
CREATE TABLE named_entities (
  id INTEGER PRIMARY KEY,
  surface_form TEXT,
  canonical_form TEXT,
  entity_type TEXT,  -- person, place, divine, etc.
  definition TEXT,
  concept_id INTEGER,
  FOREIGN KEY (concept_id) REFERENCES concepts(id)
);

CREATE TABLE entity_mentions (
  id INTEGER PRIMARY KEY,
  entity_id INTEGER,
  word_id INTEGER,
  surah INTEGER,
  verse INTEGER,
  FOREIGN KEY (entity_id) REFERENCES named_entities(id),
  FOREIGN KEY (word_id) REFERENCES quranic_words(id)
);
```

---

## PHASE 4: PRONOUN COREFERENCE (Weeks 11-12)

### 4a. Coreference Schema
```sql
CREATE TABLE coreference_links (
  id INTEGER PRIMARY KEY,
  pronoun_word_id INTEGER,
  antecedent_word_id INTEGER,
  surah INTEGER,
  verse INTEGER,
  confidence REAL,
  FOREIGN KEY (pronoun_word_id) REFERENCES quranic_words(id),
  FOREIGN KEY (antecedent_word_id) REFERENCES quranic_words(id)
);

CREATE TABLE reference_chains (
  id INTEGER PRIMARY KEY,
  entity_id INTEGER,
  word_ids TEXT,  -- JSON array of word IDs in chain
  surah INTEGER,
  verse_range TEXT  -- "2:216-2:220"
);
```

---

## TESTING & VALIDATION

### Unit Tests
```haskell
-- Test parsing
testParseTreebank = do
  let line = "1\tبِ\tب\tP\t1\tP|PREF|LEM:ب"
  case parseDependencyLine line of
    Right ann -> assert (saRelation ann == "PREP")
    Left err -> fail err

-- Test insertions
testInsertOntology = do
  conn <- open ":memory:"
  initSchema conn
  insertConcept conn testConcept
  result <- query conn "SELECT COUNT(*) FROM concepts"
  assertEqual result (Only 1)
```

### Integration Tests
```bash
# Test with sample verses
curl http://localhost:8080/api/v1/syntactic-structure/1/1
curl http://localhost:8080/api/v1/concepts/الله
curl http://localhost:8080/api/v1/entities/person

# Verify data consistency
# Check that all references are valid
# Test performance with large queries
```

### Data Validation
```bash
# For each component:
- Verify all 6,236 verses are covered
- Check no orphaned references
- Validate all relationships are bidirectional where needed
- Check encoding is correct (no mojibake)
- Verify frequency counts are reasonable
```

---

## TIMELINE SUMMARY

| Phase | Component | Duration | Start | End |
|-------|-----------|----------|-------|-----|
| **0** | Setup & Download | 1 week | Week 0 | Week 1 |
| **1** | Syntactic Treebank | 2-3 weeks | Week 1 | Week 4 |
| **2** | Semantic Ontology | 3-4 weeks | Week 4 | Week 8 |
| **3** | Dictionary + NER | 2-3 weeks | Week 8 | Week 11 |
| **4** | Pronoun Coref | 1-2 weeks | Week 11 | Week 12 |
| **5** | Testing & Deploy | 1-2 weeks | Week 12 | Week 13-14 |

**Total: 8-14 weeks** (depending on parallel work)

---

## SUCCESS CRITERIA

### Phase 1 Complete When:
- [ ] All 6,236 verses have syntactic annotations
- [ ] Dependency graphs render without errors
- [ ] API response time < 100ms
- [ ] All 8 dependency relation types working

### Phase 2 Complete When:
- [ ] All 300+ concepts loaded and linked
- [ ] 350+ relationships mapped
- [ ] Concept network renders smoothly
- [ ] Word-to-concept mapping > 80% coverage

### Phase 3 Complete When:
- [ ] Dictionary enhanced with frequency data
- [ ] All named entities tagged
- [ ] Entity browser functional
- [ ] Entity mentions linked to verses

### Phase 4 Complete When:
- [ ] Pronoun-antecedent links working
- [ ] Reference chains identified
- [ ] Coreference display functional
- [ ] Accuracy > 85% (compared to manual)

---

## BLOCKERS & CONTINGENCIES

### Potential Issues
1. **Data Format Unexpected**
   - Contingency: Parse manually from website
   - Timeline impact: +1-2 weeks

2. **File Sizes Larger Than Expected**
   - Contingency: Stream parse; use batch loading
   - Timeline impact: +1 week

3. **API Rate Limits**
   - Contingency: Cache data; batch requests
   - Timeline impact: None (download-based)

4. **Performance Issues**
   - Contingency: Add indexes; optimize queries
   - Timeline impact: +1 week

---

## Communication & Coordination

### Weekly Checkpoints
- Week 1: Data downloaded, formats understood
- Week 4: Treebank fully integrated
- Week 8: Ontology operational
- Week 11: All components integrated
- Week 14: Testing complete, ready for release

### Risks to Monitor
- Format changes between corpus versions
- Data quality issues in ontology
- Performance degradation with large graphs
- User interface complexity (too many features)

---

## Deliverables

### By End of Phase 1
- [ ] Syntactic treebank loading script
- [ ] Database schema + indexes
- [ ] API endpoints
- [ ] PureScript component for graphs
- [ ] Unit tests
- [ ] Documentation

### By End of Phase 2
- [ ] Ontology loader
- [ ] Word-concept linking algorithm
- [ ] Concept network visualization
- [ ] API integration tests
- [ ] Updated UI

### By End of Phase 3
- [ ] Enhanced dictionary with frequency
- [ ] Named entity browser
- [ ] Entity highlighting in verses

### By End of Phase 4
- [ ] Pronoun coreference display
- [ ] Reference chain visualization
- [ ] Comprehensive testing suite

---

## Next Steps

1. **This Week**: Register with corpus.quran.com and download data
2. **Next Week**: Inspect file formats and create parsing sketches
3. **Following Weeks**: Implement Phase 1 (treebank)

**Action Item**: Schedule call with team to confirm timeline and resource allocation.


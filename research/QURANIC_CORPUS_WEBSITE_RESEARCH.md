# Quranic Arabic Corpus Website Research
## corpus.quran.com - Comprehensive Feature Documentation

**Author**: Kais Dukes (Same author as the morphological corpus we're using)  
**URL**: https://corpus.quran.com  
**Research Date**: 2025-10-27  
**License**: GNU General Public License (open source)

---

## Overview

The **Quranic Arabic Corpus** is a comprehensive annotated linguistic resource providing word-by-word grammar, syntax, morphology, and semantic analysis for all 6,236 verses across 114 chapters of the Holy Quran. The website serves **over 2,500 daily users from 165 different countries** and serves as a global resource for Quranic Arabic research, language learning, and computational analysis.

**Core Mission**: Build the most accurate linguistic resource for Quranic Arabic through community participation and scholarly rigor.

---

## Primary Analysis Features

### 1. Word by Word Analysis
- **Purpose**: Maps out the complete syntax of the entire Quran with analysis and translation
- **Functionality**: 
  - Word-level grammatical breakdowns
  - English translations for each word
  - Part-of-speech identification
  - Morphological features (case, mood, person, number, gender)
  - Syntactic relationships between words
- **Interactive Elements**: Users can click on Arabic words to view detailed analysis or suggest corrections
- **Use Case**: Language learners, linguistic researchers, Quranic scholars

### 2. Quranic Grammar (إعراب - I'raab)
- **Approach**: Traditional Arabic grammatical analysis illustrated through dependency graphs
- **Grammatical Elements Covered**:
  - **Case Marking**: Nominative (nominative case), Accusative (accusative case), Genitive (genitive case), Jussive (jussive mood)
  - **Word Function**: Parts of speech classification (particles حروف, nouns أسماء, adjectives صفات, verbs أفعال)
  - **Syntactic Relationships**: How words relate to one another (connected/dependent relations متعلقان)
  - **Ellipsis**: Identification of omitted or implied elements (محذوف)
  - **Diacritical Marks**: Case indicators and vowel markings

- **Methodology**: Analysis adapted from the **Quran Printing Complex** (Madina Mushaf)
- **Presentation**: 
  - Direct Arabic notation for each word's classification
  - English transliteration and translation of grammatical terms
  - Cross-references to morphology and dependency graphs
- **Learning Method**: Breaks down grammar concepts using actual Quranic verses as examples

### 3. Syntactic Treebank
- **Purpose**: Maps out the complete grammar of the entire Quran using dependency relationships
- **Methodology**: Uses mathematical graph theory and dependency parsing
- **Functionality**:
  - Visualizes grammatical dependencies as dependency graphs
  - Shows how Arabic words link together through syntactic relationships
  - Displays verb-argument structures
  - Maps modifier-head relationships
  - Shows coordination and subordination patterns

- **Visual Representation**: Interactive dependency graphs that illustrate sentence structure
- **Scope**: Comprehensive coverage of all 6,236 Quranic verses
- **Research Applications**: Syntactic pattern analysis, computational linguistics, NLP training

### 4. Semantic Ontology of Concepts
- **Definition**: A knowledge representation system that "defines the key concepts in the Quran and shows relationships between these concepts using predicate logic"

- **Scale**: 
  - Approximately **300 linked concepts**
  - **350 semantic relations** connecting them
  - Grounded in traditional Islamic scholarship

- **Conceptual Foundation**:
  - Based on **hadith of Prophet Muhammad** (Prophetic traditions)
  - Based on **tafsir (Quranic exegesis) of Ibn Kathir** (medieval Islamic scholar)
  - Ensures concepts align with established Quranic interpretation

- **Primary Relationship Types**:
  - **Instance relations**: Examples like linking Satan to the jinn (a sentient creation)
  - **Category groupings**: Related concepts grouped by shared properties
    - Example: Sun, Earth, Moon grouped under "Astronomical Body"
  - **Property relationships**: Semantic associations between concepts

- **Three Layers of Annotation**:

  1. **Pronoun Resolution**: 
     - Pronouns linked to ontology concepts
     - Clarifies implicit references (e.g., "it" refers to specific concept)
     - Disambiguates pronoun antecedents

  2. **Named Entity Tagging**:
     - Proper nouns tagged and linked to ontology
     - Examples: "Night of Decree" (Laylat al-Qadr), "Day of Judgment"
     - Geographic entities, person names, event names

  3. **Morphological Analysis**:
     - Word-level grammatical details support semantic disambiguation
     - Links morphology to semantic concepts

- **Example Concepts**: Religious concepts (Allah, Prophet, Faith), Natural phenomena (Sun, Moon, Water), Human actions (believing, doubting, obeying), Abstract concepts (mercy, justice, knowledge)

---

## Quranic Dictionary

- **Purpose**: Lexical reference tool for Quranic vocabulary
- **Functionality**:
  - Word definitions and meanings
  - Root-based organization
  - Semantic field groupings
  - Usage frequency in the Quran
  - Cross-references to where words appear in verses

- **Integration**: Connected to word-by-word analysis for seamless lookups
- **Target Users**: Researchers, students, language learners

---

## English Translation

- **Scope**: Complete English translation of all 114 chapters and 6,236 verses
- **Presentation**: Rendered alongside Arabic text for side-by-side comparison
- **Integration**: Available at verse level alongside morphological and syntactic analysis

---

## Data Corpus Structure

### Quranic Content Breakdown
- **Total Verses**: 6,236 numbered verses (آيات)
- **Total Chapters**: 114 chapters (سور)
- **Script**: Uthmani orthography (standard Islamic manuscript tradition)
- **Diacritical Marks**: Full diacritics and vowel markings included

### Data Annotation Levels

The corpus provides **three comprehensive levels of linguistic annotation**:

1. **Morphological Annotation**
   - Word-by-word grammatical features
   - Root identification
   - Lemmatization
   - Part-of-speech tagging
   - Inflectional features

2. **Syntactic Treebank**
   - Dependency parsing
   - Syntactic relations between words
   - Clause structure analysis
   - Argument structure

3. **Semantic Ontology**
   - Concept linking and resolution
   - Named entity tagging
   - Pronoun coreference
   - Semantic relationships

---

## User-Facing Features & Interaction Methods

### Navigation & Browsing
- **Chapter-Verse Navigation**: Access verses using chapter:verse notation (e.g., 2:216, 21:30)
- **Sequential Browsing**: Navigate through chapters and verses sequentially
- **Search Functionality**: Query specific terms (detailed search help available at `/searchhelp.jsp`)
- **Interactive Verse Display**: Verses presented in interactive, annotated format

### Interactive Elements
- **Clickable Words**: Users can click on Arabic words to view detailed analysis
- **Correction Suggestions**: Users can suggest corrections online for inaccurate analyses
- **Community Feedback**: Message board for discussing Arabic language and grammatical analysis
- **User Accounts**: Create accounts to participate in collaborative improvement efforts

### Information Display per Verse
For each verse, users see:
1. **Arabic Text** (clickable with full diacritics)
2. **English Translation** (parallel display)
3. **Morphological Breakdown**: Grammar, syntax, word-by-word definitions
4. **Syntactic Dependency Graphs**: Visual representation of grammatical relationships
5. **Semantic Annotations**: Concept links and named entities (where applicable)
6. **Grammar Analysis (إعراب)**: Traditional grammatical classification

---

## Data Access & Distribution

### Downloadable Resources
- **Morphological Data**: Version 0.4 of Quranic Arabic Corpus morphological dataset
- **Access**: Email contact required to download
- **Available Datasets**:
  - Word by Word annotations
  - Quran Dictionary
  - English Translation
  - Syntactic Treebank
  - Ontology of Concepts

### File Formats
- **Distribution**: Available in multiple formats (specific formats not detailed on website)
- **License**: GNU General Public License (GPL)
- **Attribution Requirement**: Source (Quranic Arabic Corpus) must be clearly indicated when used in any website or application

### Java API (JQuranTree)

**Purpose**: Programmatic access for developers and computational linguists

**Open Source Status**: Released as open source to encourage computational analysis of the Quran

**Exposed Functionality**:
1. **Text Representations**:
   - Uthmani script (authentic Islamic manuscript form)
   - Simple encoding
   - Unicode serialization
   - Transliteration support

2. **Linguistic Analysis Tools**:
   - Orthography modeling
   - Syntactic treebank access
   - Grammatical analysis
   - Token-level searching

3. **Word Processing**:
   - Transliteration generation
   - Character normalization
   - Token-level analysis

4. **Integrated Resources**:
   - Linguistic data tables
   - Quranic dictionary integration
   - Morphological analysis tables

**Documentation Available**:
- JavaDoc API documentation
- Orthography internals reference
- Build instructions
- Multiple code examples
- Test coverage information

**Use Cases**: 
- Computational text analysis
- Linguistic research tools
- Educational applications
- NLP training and development

---

## Community & Support Features

### Message Board
- **Purpose**: Venue for discussing Arabic language and grammatical analysis
- **Community**: Users from 165 countries participating
- **Topics**: Linguistic questions, grammatical analysis discussions, analysis corrections

### Documentation
- **Comprehensive Guides**: Available for users and developers
- **Search Help**: Detailed search syntax documentation (`/searchhelp.jsp`)
- **Grammar Guide**: Traditional Arabic grammar guide with examples
- **API Documentation**: Complete developer documentation for Java API

### Feedback & Correction System
- **User Contribution**: Community can suggest corrections directly
- **Workflow**: Click on words → suggest analysis improvements
- **Editorial Review**: Submitted corrections reviewed for accuracy
- **Mission**: Continuously improve corpus accuracy through community participation

### FAQ Section
- **Coverage**: Answers to common research questions
- **Accessibility**: Help for new and experienced users

### User Accounts
- **Registration**: Users can create accounts
- **Benefits**: Participate in community feedback, track contributions
- **Engagement**: Collaborative improvement of the corpus

---

## Integration with Ecosystem

### Related Platforms
The corpus is part of a larger Islamic knowledge ecosystem:

1. **Quran.com** 
   - General Quranic reading platform
   - Integration with corpus data

2. **Quranic Audio**
   - Audio recitations of the Quran
   - Complements written analysis

3. **Prayer Times Applications**
   - Islamic practice support
   - Part of broader Islamic digital infrastructure

### Research Collaboration
- **Scholarly Input**: Based on traditional Islamic scholarship
- **Continuous Improvement**: Community-driven enhancement
- **Academic Integration**: Used in linguistics and Islamic studies research

---

## Technical Specifications

### Scope & Scale
- **Verses Analyzed**: 6,236 (complete Quranic corpus)
- **Chapters Covered**: 114 (entire Quran)
- **Daily Users**: 2,500+
- **Geographic Reach**: 165 countries

### Data Quality
- **Scholarly Basis**: Grounded in Islamic scholarship (Ibn Kathir tafsir, hadith traditions)
- **Community Validation**: Continuous correction and improvement by users
- **Standardized Reference**: Based on Madina Mushaf (official Islamic printed edition)

### Technology
- **Web Platform**: Java-based web application
- **Query Interface**: Browser-based access with interactive features
- **API Access**: Java library for programmatic access
- **Graph Visualization**: Dependency graphs for syntactic display

---

## Comparison with Our Aralex Project

### What the Corpus Website Offers that We Can Learn From

| Feature | Corpus.quran.com | Aralex | Notes |
|---------|-----------------|--------|-------|
| Morphological Analysis | ✅ Full implementation | ✅ Using same data | Same source (Kais Dukes) |
| Syntactic Treebank | ✅ Interactive display | ❌ Not yet | Could be implemented using their methodology |
| Semantic Ontology | ✅ 300+ concepts | ❌ Not yet | Opportunity for expansion |
| Dictionary Integration | ✅ Linked dictionary | ✅ 6 dictionaries | We have more dictionaries than they reference |
| Phonosemantic Analysis | ❌ Not in corpus | ✅ Hassan Abbas theory | Our unique contribution |
| Interactive Corrections | ✅ User suggestions | ❌ Not yet | Could add user feedback system |
| Java API | ✅ Available | ❌ Not yet | Could expose our own API |
| Community Features | ✅ Message board | ❌ Not yet | Could add forum/discussion features |
| Pronunciation Guide | ❌ Linked only | ❌ Not yet | Could integrate with Quranic audio |

### Inspiration for Aralex Development

1. **Interactive Word Clicking**: Already implemented in our frontend
2. **Multi-Level Analysis Display**: We show morphology, phonosemantics, dictionary, and etymology
3. **Graph Visualization**: Etymology graph uses similar graph-based approach
4. **Integrated Dictionary**: Our 6 classical dictionaries provide richer lexical data
5. **Community Contribution**: Could add user feedback mechanism for phonosemantic patterns
6. **API Development**: Could expose REST API for third-party tools
7. **Semantic Linking**: Could expand pronoun resolution and entity linking
8. **Educational Focus**: Both projects serve scholars, students, and researchers

---

## Development Opportunities for Aralex

Based on corpus.quran.com's features, Aralex could potentially add:

1. **Syntactic Visualization**: Implement dependency graph display alongside morphology
2. **Named Entity Recognition**: Link named entities to entities in the Quran
3. **Pronoun Coreference**: Add pronoun resolution annotation
4. **Community Contributions**: Implement user feedback system for phonosemantic patterns
5. **API Development**: Expose REST/GraphQL API for third-party applications
6. **Message Board Integration**: Add discussion features for linguistic analysis
7. **Advanced Search**: Implement search by phonosemantic patterns
8. **Pattern Analysis**: Search by letter combinations and their semantic effects
9. **Historical Etymology**: Link classical dictionary entries to Quranic usage
10. **Interactive Corrections**: Allow users to suggest corrections to phonosemantic analyses

---

## Key Differences: Our Aralex vs. Corpus Website

### What Makes Aralex Unique

1. **Phonosemantic Focus**: Hassan Abbas's letter-meaning theory (not in corpus)
2. **Classical Dictionary Integration**: 6 dictionaries (corpus doesn't emphasize this)
3. **Etymology Graphs**: Force-directed visualization of root relationships
4. **Visual Design**: Modern PureScript/Halogen frontend vs. their traditional web app
5. **Specialized Analysis**: Letter-by-letter semantic breakdown
6. **Historical Linguistics**: Direct connection between classical scholarship and modern analysis

### Potential Synergies

1. We could implement their **syntactic treebank** approach for Quranic grammar
2. We could expand their **semantic ontology** with our **phonosemantic patterns**
3. We could provide **richer dictionary data** (6 vs. their referenced sources)
4. We could **link their pronunciation guide** to our etymology system
5. We could **implement their correction system** for collaborative improvement

---

## Research Conclusion

The **Quranic Arabic Corpus** (corpus.quran.com) is a mature, well-established platform offering comprehensive linguistic analysis at three levels: morphological, syntactic, and semantic. Its strength lies in:

1. **Scholarly rigor** (based on traditional Islamic scholarship)
2. **Comprehensive coverage** (entire Quranic corpus, 6,236 verses)
3. **Community validation** (2,500+ daily users providing feedback)
4. **Multiple access methods** (web interface, Java API, downloadable data)
5. **Linguistic depth** (dependency graphs, grammatical analysis, semantic linking)

Our **Aralex project** complements this by adding:
- **Phonosemantic analysis** (unique theoretical framework)
- **Classical dictionary integration** (deeper historical linguistics)
- **Visual etymology** (force-directed graphs)
- **Modern UX** (interactive frontend, web-based interface)

Together, these resources provide complementary perspectives on Quranic Arabic: the corpus website offers comprehensive linguistic analysis, while Aralex adds specialized phonosemantic interpretation grounded in Hassan Abbas's theory and classical Islamic lexicography.

---

## References & Further Reading

- **Website**: https://corpus.quran.com
- **Java API**: https://corpus.quran.com/java/
- **Download Resources**: https://corpus.quran.com/download/
- **Creator**: Kais Dukes (author of Quranic morphological corpus)
- **License**: GNU General Public License (open source)
- **Related**: Quran.com (reading platform), Quranic Audio recordings

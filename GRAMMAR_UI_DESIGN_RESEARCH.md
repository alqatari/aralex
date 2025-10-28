# Grammar Display UI/UX Design Research

**Date**: 2025-10-27  
**Source**: corpus.quran.com (by Kais Dukes et al.)

## Overview

Research on how the Quranic Arabic Corpus website displays traditional Arabic grammar (إعراب) to inform our Aralex frontend implementation.

## Key Design Patterns from corpus.quran.com

### 1. Two-Column Layout

**Left Column**:
- Arabic word with full diacritics
- English translation (parentheses)
- Clickable links to dictionary/morphology

**Right Column**:
- Grammatical breakdown (English terms)
- Arabic grammatical terminology (جار ومجرور)
- Case/gender/number/form information

### 2. Word-Level Analysis Display

Each word shows hierarchical information:

```
بِسْمِ
├─ [P] Prefixed preposition (bi)
├─ [N] Genitive masculine noun
├─ Translation: "in (the) name"
└─ Arabic: جار ومجرور
```

### 3. Typography Hierarchy

- **Bold**: Grammatical category labels (N, V, P, ADJ)
- **Italic**: Technical terms (genitive, prefixed, nominative)
- **Regular**: Arabic text and translations
- **Color coding**: Different parts of speech

### 4. Interactive Elements

1. **Clickable words** → Full dictionary entry
2. **Morphology links** → Detailed segment breakdown
3. **Tabs**: Word by Word | Dictionary | Translation | Treebank
4. **Navigation**: Verse-by-verse with chapter/verse selectors

### 5. Arabic Text Presentation

- Full Uthmani script with diacritics
- Romanization alongside Arabic (bis'mi, ٱللَّهِ)
- Preserves traditional orthography
- Clear, readable font (likely Scheherazade or similar)

### 6. Traditional إعراب Display

**Bilingual approach**:
- English grammatical terms for accessibility
- Arabic grammatical terms for traditional scholars
- Both displayed simultaneously

Example:
```
N – genitive masculine noun
جار ومجرور (genitive prepositional phrase)
```

## Design Recommendations for Aralex

### Proposed Layout

```
┌──────────────────────────────────────────────────────┐
│  Verse Display (Arabic with highlighting)            │
└──────────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────────┐
│  Word Analysis Panel (Collapsible)                   │
│  ┌────────────────┬──────────────────────────────┐  │
│  │ بِسْمِ          │ [إعراب] Grammar              │  │
│  │ "in the name"  │ • النوع: اسم (Noun)          │  │
│  │                │ • الحالة: مجرور (Genitive)   │  │
│  │ [Root: سمو]    │ • العدد: مفرد (Singular)     │  │
│  │ [Letters: ب س م]│ • الجنس: مذكر (Masculine)    │  │
│  └────────────────┴──────────────────────────────┘  │
└──────────────────────────────────────────────────────┘
```

### Visual Design Elements

#### 1. Color Scheme (Arabic-Friendly)

**Part of Speech Colors**:
- Nouns (اسم): Deep blue #2C5282
- Verbs (فعل): Green #2F855A
- Particles (حرف): Purple #6B46C1
- Proper nouns (علم): Gold #D69E2E

**Grammar Feature Colors**:
- Case markers (إعراب): Subtle gray backgrounds
- Particle families (إن وأخواتها): Highlighted in amber
- Passive voice: Light red tint

#### 2. Typography

**Arabic Text**:
- Primary: Scheherazade New (24-28px)
- Headers: Lalezar or Aref Ruqaa
- Body: Readex Pro or Noto Naskh Arabic

**Latin Text**:
- Headers: Roboto Condensed
- Body: Roboto Flex

#### 3. Interactive Elements

**Click Behaviors**:
- Click word → Show detailed analysis panel
- Click root → Show etymology graph
- Click letter → Show phonosemantic meaning
- Hover word → Tooltip with quick grammar info

**Expandable Sections**:
```
▼ إعراب تفصيلي (Detailed Grammar)
  ├─ Basic Info (always visible)
  ├─ Verb Conjugation (expandable)
  ├─ Particle Family (expandable)
  └─ Etymology (expandable)
```

### 4. Grammar Display Components

#### Component A: Basic Grammar Badge

Small inline display next to word:

```
الْحَمْدُ [اسم • مرفوع • مفرد]
```

#### Component B: Detailed Grammar Panel

Full breakdown when word is selected:

```
┌─────────────────────────────────────┐
│ إعراب الكلمة: الْحَمْدُ              │
├─────────────────────────────────────┤
│ النوع: اسم (Noun)                   │
│ الحالة: مرفوع (Nominative)          │
│ العدد: مفرد (Singular)              │
│ الجنس: مذكر (Masculine)             │
│ التعريف: معرف بأل (Definite)        │
│                                     │
│ الجذر: حمد (h-m-d)                  │
│ المعنى: praise, thanks              │
└─────────────────────────────────────┘
```

#### Component C: Verb Conjugation Panel

For verbs, show complete analysis:

```
┌─────────────────────────────────────┐
│ تحليل الفعل: يُؤْمِنُونَ             │
├─────────────────────────────────────┤
│ النوع: فعل مضارع (Imperfect verb)  │
│ الوزن: الرابع (Form IV - أَفْعَلَ)  │
│ الجذر: أمن (to believe)             │
│ الصيغة: معلوم (Active voice)        │
│ المزاج: مرفوع (Indicative mood)     │
│ الضمير: هم (3rd person masc. pl.)  │
│                                     │
│ التصريف: آمَنَ، يُؤْمِنُ، آمِنْ      │
└─────────────────────────────────────┘
```

#### Component D: Particle Family Highlight

Special treatment for grammatical families:

```
┌─────────────────────────────────────┐
│ ⚠️  إِنَّ - من إن وأخواتها          │
├─────────────────────────────────────┤
│ الوظيفة: حرف توكيد ونصب            │
│ العمل: تنصب الاسم وترفع الخبر      │
│                                     │
│ الأخوات: إنّ، أنّ، كأنّ، لكنّ، ليت │
└─────────────────────────────────────┘
```

### 5. Mobile Responsive Design

**Stacked Layout** on small screens:

```
┌────────────────┐
│ الْحَمْدُ       │
│ "the praise"   │
├────────────────┤
│ [Expand إعراب] │
└────────────────┘
  ↓ (when expanded)
┌────────────────┐
│ Grammar Panel  │
│ Full details   │
└────────────────┘
```

## Implementation Priority

### Phase 1: Core Display (Week 1-2)
✅ Word-level grammar display
✅ Basic case/gender/number
✅ POS color coding
✅ Clickable expansion

### Phase 2: Advanced Features (Week 3-4)
- Verb conjugation panel
- Particle family highlighting
- Hover tooltips
- Arabic grammar terminology

### Phase 3: Polish (Week 5-6)
- Animations/transitions
- Advanced filtering (show only verbs, etc.)
- Print/export functionality
- Accessibility features

## Design Mockup Ideas

### Option 1: Inline Badges (Minimal)

```
الْحَمْدُ [اسم مرفوع] لِلَّهِ [اسم مجرور] رَبِّ [اسم مجرور] الْعَالَمِينَ [اسم مجرور]
```

**Pros**: Clean, doesn't obscure text, quick overview  
**Cons**: Limited detail, requires click for full info

### Option 2: Below-Word Display (Detailed)

```
الْحَمْدُ      لِلَّهِ       رَبِّ         الْعَالَمِينَ
─────────     ──────       ─────        ────────────
اسم مرفوع     اسم مجرور    اسم مجرور    اسم مجرور
مفرد مذكر     علم          مضاف         جمع مذكر
```

**Pros**: Shows everything at once, traditional  
**Cons**: Takes vertical space, can be overwhelming

### Option 3: Sidebar Panel (Hybrid) - **RECOMMENDED**

```
┌──────────────────────────┬─────────────────────┐
│ الْحَمْدُ لِلَّهِ رَبِّ    │  إعراب الكلمة      │
│ الْعَالَمِينَ             │                     │
│                          │  [Selected: الْحَمْدُ]│
│ الرَّحْمَٰنِ الرَّحِيمِ     │  • اسم (Noun)       │
│ مَالِكِ يَوْمِ الدِّينِ     │  • مرفوع (Nom)      │
│                          │  • مفرد (Sing)      │
│ [Click any word]         │  • مذكر (Masc)      │
└──────────────────────────┴─────────────────────┘
```

**Pros**: Clean verse display, detailed when needed, flexible  
**Cons**: Requires wider screen for desktop

## Technical Implementation Notes

### Halogen Components Structure

```purescript
-- Main component
component :: H.Component Query Input Output m

data GrammarDisplayMode
  = MinimalBadges
  | DetailedPanel
  | SidebarView

-- Sub-components
wordGrammarBadge :: Word -> HTML
grammarDetailPanel :: MorphFeatureDTO -> HTML
particleFamilyHighlight :: ParticleFamily -> HTML
verbConjugationTable :: VerbFeatures -> HTML
```

### CSS Classes for Grammar Features

```css
.grammar-badge { /* Inline badges */ }
.grammar-panel { /* Detailed panel */ }
.pos-noun { color: #2C5282; }
.pos-verb { color: #2F855A; }
.pos-particle { color: #6B46C1; }
.case-nominative { /* مرفوع */ }
.case-accusative { /* منصوب */ }
.case-genitive { /* مجرور */ }
.particle-family-highlight { background: #FEF3C7; }
```

## User Experience Flow

1. **Initial Load**: Verse displayed with minimal badges
2. **Click Word**: Sidebar/panel opens with full grammar
3. **Hover Word**: Tooltip shows quick grammar summary
4. **Toggle Mode**: Switch between minimal/detailed views
5. **Filter**: Show only specific POS or features
6. **Export**: Copy grammar analysis to clipboard

## Accessibility Considerations

- Screen reader support for Arabic grammatical terms
- Keyboard navigation (Tab through words)
- High contrast mode support
- Font size adjustability
- RTL (Right-to-Left) proper handling

---

## Next Steps for Implementation

1. ✅ Types complete (Haskell + Purescript)
2. ✅ API ready (serving all grammar features)
3. ⏳ Design mockups (create visual prototypes)
4. ⏳ Halogen components (implement UI)
5. ⏳ CSS styling (apply design system)
6. ⏳ User testing (validate with Arabic learners)

**Recommendation**: Start with **Option 3 (Sidebar Panel)** for desktop, gracefully degrade to expandable sections on mobile.

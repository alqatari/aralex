# Grammar Display Testing Guide

**Quick Start**: How to test the new grammar display feature

---

## Prerequisites

1. Backend built: `cd backend && cabal build`
2. Frontend built: `cd frontend && npm run build`
3. Data loaded: Morphology database populated

---

## Step-by-Step Testing

### 1. Start the Backend

```bash
cd backend
cabal run aralex-backend
```

**Expected Output**:
```
Starting Aralex backend server on port 8080...
Databases loaded successfully
Server running at http://localhost:8080
```

### 2. Test API Directly (Optional)

```bash
# Test morphology endpoint
curl http://localhost:8080/api/v1/morphology/1/1 | jq

# Expected: Array of QuranicWordDTO with all grammar features
# Should see: dtoWordPOS, dtoSegments, dtoFeatures, etc.
```

### 3. Open Frontend in Browser

**Option A**: Using build output
```bash
# Backend serves frontend from dist/
open http://localhost:8080
```

**Option B**: Using dev server (hot reload)
```bash
cd frontend
npm run dev
# Opens http://localhost:3000
```

---

## Test Scenarios

### Scenario 1: Basic Grammar Display

1. **Search**: Enter `كتب` in search box
2. **Expand**: Click on any Surah group (e.g., البقرة)
3. **Observe**: Verses should now show individual clickable words
4. **Hover**: Hover over a word
   - **Expected**: Word background changes color
   - **Expected**: Tooltip shows النوع (اسم/فعل/حرف)
5. **Click**: Click on a word
   - **Expected**: Grammar panel opens below the verse
   - **Expected**: Panel shows:
     - Header with word (colored by POS)
     - النوع (type)
     - الجذر (root) 
     - اللمة (lemma)
     - Feature badges (مرفوع، مفرد، etc.)

### Scenario 2: Different Word Types

Test with different parts of speech:

**Test Noun** (e.g., الْحَمْدُ in 1:2):
- Color: Blue
- Features: مرفوع، مفرد، مذكر، معرفة

**Test Verb** (e.g., يُؤْمِنُونَ in 2:3):
- Color: Green  
- Features: مضارع، الوزن الرابع، مبني للمعلوم، مرفوع

**Test Particle** (e.g., بِ in 1:1):
- Color: Purple
- Features: حرف جر

### Scenario 3: Multiple Words

1. Click on word A → Panel opens for A
2. Click on word B → Panel switches to B
3. Click close button → Panel closes
4. Click on word A again → Panel reopens for A

### Scenario 4: Different Verses

1. Search: `قال`
2. Expand multiple Surahs
3. Test grammar in different verses
4. Verify morphology fetched for each verse

---

## What to Look For

### ✅ Success Indicators

- [x] Words are clickable and separate (not one block of text)
- [x] Hover shows background color change
- [x] Hover tooltip shows Arabic النوع
- [x] Click opens grammar panel
- [x] Panel header colored by POS (blue/green/purple)
- [x] Panel shows Arabic labels only (no English)
- [x] Feature badges display correctly (مرفوع، مفرد, etc.)
- [x] Close button works
- [x] Clicking different words switches panel
- [x] Panel appears below the selected word's verse

### ❌ Issues to Watch For

- [ ] Words not clickable → Check morphology fetched
- [ ] Panel doesn't open → Check console for errors
- [ ] English labels appear → Check Util.Grammar functions
- [ ] Colors wrong → Check Grammar.posColor
- [ ] Features missing → Check parser/DTO mapping
- [ ] Panel doesn't close → Check CloseGrammarPanel action

---

## Console Debugging

Open browser console (F12) and check for:

```javascript
// Morphology fetch logs
"Fetching morphology for 1:1"
"Morphology cached for 1:1"

// Action logs (if logging enabled)
"Action: SelectWordForGrammar"
"Action: CloseGrammarPanel"
```

---

## Expected Grammar Display Examples

### Example 1: الْحَمْدُ (Al-Fatiha 1:2)

```
┌──────────────────────────────────────┐
│ إعراب: الْحَمْدُ              [Close] │
│ [Blue header - اسم]                  │
├──────────────────────────────────────┤
│ النوع: اسم                           │
│ الجذر: حمد                           │
│ اللمة: حَمْد                         │
│                                      │
│ التفاصيل النحوية:                    │
│ [مرفوع] [مفرد] [مذكر] [معرفة]       │
└──────────────────────────────────────┘
```

### Example 2: يُؤْمِنُونَ (Al-Baqara 2:3)

```
┌──────────────────────────────────────┐
│ إعراب: يُؤْمِنُونَ            [Close] │
│ [Green header - فعل]                 │
├──────────────────────────────────────┤
│ النوع: فعل                           │
│ الجذر: أمن                           │
│ اللمة: آمَنَ                         │
│                                      │
│ التفاصيل النحوية:                    │
│ [مضارع] [الوزن الرابع]              │
│ [مبني للمعلوم] [مرفوع]              │
│ [الغائب المذكر الجمع]               │
└──────────────────────────────────────┘
```

---

## Performance Testing

### Check Response Times

1. Open Network tab in browser devtools
2. Expand a Surah
3. Look for `/api/v1/morphology/X/Y` requests
4. **Expected**: < 100ms per verse
5. **Expected**: Subsequent clicks instant (cached)

### Check Memory Usage

1. Open Performance/Memory tab
2. Expand several Surahs
3. Click many words
4. **Expected**: Memory stable, no leaks
5. **Expected**: < 50 MB for typical usage

---

## Mobile Testing (Optional)

1. Open browser responsive mode (F12 → Toggle device toolbar)
2. Select mobile device (iPhone, iPad, etc.)
3. Test same scenarios as desktop
4. **Expected**: Panel appears below verse (not sidebar)
5. **Expected**: Touch interactions work
6. **Expected**: Readable font sizes

---

## Troubleshooting

### Problem: Words not clickable

**Solution**:
- Check if morphology fetched (console logs)
- Verify Surah expanded (triggers fetch)
- Check network tab for API calls
- Verify backend running on :8080

### Problem: Panel doesn't display

**Solution**:
- Open console, look for errors
- Check if `selectedWordForGrammar` in state
- Verify `renderGrammarPanel` called
- Check CSS not hiding panel

### Problem: Wrong Arabic labels

**Solution**:
- Check `Util.Grammar.purs` functions
- Verify feature mapping in `featureToShortArabic`
- Test specific feature in console

### Problem: Morphology not loading

**Solution**:
```bash
# Test API directly
curl http://localhost:8080/api/v1/morphology/1/1

# Should return JSON array
# If empty, check backend database
sqlite3 backend/aralex.db "SELECT COUNT(*) FROM quranic_words"
```

---

## Reporting Issues

When reporting issues, include:

1. **Browser**: Chrome/Firefox/Safari + version
2. **Console errors**: Copy from F12 console
3. **Network errors**: From F12 Network tab
4. **Steps to reproduce**: Exact click sequence
5. **Expected vs actual**: What should happen vs what happened
6. **Screenshot**: If UI-related

---

## Success Checklist

After testing, verify:

- [x] Backend starts without errors
- [x] Frontend loads in browser
- [x] Search works (e.g., `كتب`)
- [x] Surahs expand/collapse
- [x] Words are clickable
- [x] Hover shows effects
- [x] Grammar panel opens
- [x] Arabic labels correct
- [x] Colors display properly
- [x] Close button works
- [x] Multiple words testable
- [x] Performance acceptable

---

**Ready to Test!** 🚀

Any issues found should be documented with steps to reproduce.

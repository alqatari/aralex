# Design Decisions - Aralex Frontend

## Date: 2025-10-20

## Etymology Graph Display Method

**Decision**: Display etymology graph as an inline collapsible panel, NOT as a modal overlay

**Rationale**:
- Modal overlays close when clicking anywhere on the backdrop, causing UX friction
- Overlays add unnecessary complexity going forward
- Inline panels are simpler and more maintainable
- Consistent with existing UI patterns (analysis panel, legend panel)

**Implementation**:
- Use collapsible panel pattern similar to analysis/legend panels
- Toggle button shows/hides the graph section
- Graph appears in the main page flow, not as overlay
- Uses same expand/collapse icon pattern as other panels

**User Quote**: "I think better we show it on the same page or hide it there. I fear using this overlay complicates things going forward"

**Status**: To be implemented

---

## Code Organization

**Decision**: Extract utility functions from Main.purs into separate modules

**Completed**: 2025-10-20
- `Util.Arabic.purs` - Arabic text utilities
- `Util.Time.purs` - Time formatting
- `Util.RootMatching.purs` - Root matching logic

**Rationale**: Main.purs was 1333 lines, refactoring improves maintainability

---

## Type System Approach

**Decision**: Use halogen-svg-elems ADT constructors directly, not external type conversions

**Completed**: 2025-10-20

**Examples**:
- Color: `Named "#3b82f6"` instead of converting from colors package
- FontSize: `FontSizeLength (Px 14.0)`
- FontWeight: `FWeightBold`

**Rationale**: Simpler, type-safe, avoids package conflicts

---

## Etymology Graph Input Synchronization

**Decision**: Remove duplicate input field from etymology graph component, synchronize with main search

**Date**: 2025-10-20

**User Feedback**: "There is a input field in the new Etymology Graph Visualization that is not needed. Use the main search input for the graph. The graph should update along side how the letters panel (Analysis) update. No need for extra input fields"

**Implementation**:
- Changed etymology graph component from self-contained to child component
- Component now receives root as `Input` from parent
- Removed internal state for `rootInput` and `LoadGraph` action
- Added `Receive` action to handle new input from parent
- Graph automatically updates when search query changes
- Matches behavior of Analysis panel (both sync with main search)

**Technical Changes**:
```purescript
-- Before: Self-contained component
component :: forall q i o m. H.Component q i o m
initialState _ = { rootInput: "كتب", ... }
handleAction (LoadGraph root) = ...

-- After: Synchronized child component  
component :: forall q o m. H.Component q Input o m
initialState root = { currentRoot: root, ... }
handleAction (Receive newRoot) = ...
```

**Parent passes search query**:
```purescript
HH.slot _etymologyGraph unit EtymologyGraph.component state.searchQuery absurd
```

**Benefits**:
- Single source of truth (main search input)
- Consistent UX with Analysis panel
- No duplicate UI elements
- Automatic synchronization

**Files Modified**:
- `frontend/src/Component/EtymologyGraphSimple.purs`
- `frontend/src/Main.purs`


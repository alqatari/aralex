# Phase 4B: SVG Etymology Graph Visualization - COMPLETE

## Date: 2025-10-20

## Summary
Successfully implemented interactive SVG-based etymology graph visualization for the Aralex frontend using Halogen SVG elements.

## Completed Tasks

### 1. Code Refactoring
- Extracted utility functions from Main.purs (1333 lines) into separate modules:
  - `Util.Arabic.purs` - Arabic text utilities (isArabicChar, removeTashkeel, etc.)
  - `Util.Time.purs` - Time formatting
  - `Util.RootMatching.purs` - Root matching logic
- Cleaned up unused imports
- Successfully built and tested refactored code

### 2. Etymology Graph Component Enhancement
Enhanced `Component/EtymologyGraphSimple.purs` with:
- **SVG Visualization**: Replaced text-only display with interactive SVG graph
- **Interactive Controls**:
  - Zoom In/Out buttons
  - Reset View button
  - Node selection on click
- **Graph Rendering**:
  - Nodes: Different colors and sizes by type (TargetRoot, LetterNode, RelatedRoot)
  - Edges: Visual distinction between Contains and SharesLetter relationships
  - Arabic labels on nodes
  - Zoom functionality with ViewBox calculations

### 3. Type System Deep Dive & Resolution

#### Color Type Conflict (SOLVED)
**Problem**: Two incompatible `Color` types:
- `purescript-colors` package: `Color` (HSL-based, complex)
- `halogen-svg-elems`: `Halogen.Svg.Attributes.Color` (ADT-based, simple)

**Solution**: Use `halogen-svg-elems`' Color ADT constructors:
```purescript
data Color = RGB Int Int Int | RGBA Int Int Int Number | Named String | NoColor

-- Usage:
svgNodeColor TargetRoot = Named "#3b82f6"  -- Simple hex string!
```

#### FontSize Type (SOLVED)
**Problem**: `fontSize` expects `FontSize` type, not `Number`

**Solution**: Use proper constructors:
```purescript
data FontSize = ... | FontSizeLength CSSLength
data CSSLength = ... | Px Number | ...

-- Usage:
SA.fontSize (FontSizeLength (Px 14.0))
```

#### FontWeight Type (SOLVED)
**Problem**: `fontWeight` expects `FontWeight` type, not `String`

**Solution**:
```purescript
data FontWeight = FWeightNormal | FWeightBold | FWeightBolder | FWeightNumber Number

-- Usage:
SA.fontWeight FWeightBold
```

### 4. Spago Configuration Migration
**Issue Found**: `spago.yaml` was using outdated snake_case format
**Change**: `package_set:` → `packageSet:` (camelCase)
**Result**: Migration completed automatically with `--migrate` flag

## Technical Implementation

### Graph Data Structure
```purescript
type GraphNode = {
  nodeId :: String,
  nodeType :: NodeType,
  arabicText :: String,
  meaning :: Maybe String,
  occurrences :: Maybe Int,
  position :: NodePosition  -- posX, posY from backend
}

type GraphEdge = {
  edgeId :: String,
  edgeType :: EdgeType,
  fromNodeId :: String,
  toNodeId :: String,
  strength :: Number,
  label :: Maybe String
}
```

### Color Scheme
- **TargetRoot**: Blue (#3b82f6)
- **LetterNode**: Green (#10b981)
- **RelatedRoot**: Indigo (#6366f1)
- **SemanticConcept**: Amber (#f59e0b)
- **Contains Edge**: Dark gray (#1f2937, 2px)
- **SharesLetter Edge**: Light gray (#9ca3af, 1px)

### Zoom Implementation
```purescript
calculateViewBox :: Number -> ViewBox
calculateViewBox zoom =
  { minX: -400.0 / zoom
  , minY: -300.0 / zoom
  , width: 800.0 / zoom
  , height: 600.0 / zoom
  }
```

## Dependencies Added
- `halogen-svg-elems` - SVG rendering for Halogen

## Dependencies Removed
- `colors` - Not needed (type conflict)
- `partial` - Not needed after removing `unsafePartial`

## Build Status
✅ **Build Succeeded**
- Output: `frontend/dist/index.js` (246 KB)
- Warnings: 5 (unused variables - non-critical)
- Errors: 0

## Testing
- ✅ Backend serving updated bundle (246,352 bytes)
- ✅ Etymology graph API endpoint responding correctly
- ✅ Frontend loads without errors

## Key Learnings

1. **Always check source code**: The `Named String` constructor for Color was the simple solution
2. **Strongly-typed ADTs**: halogen-svg-elems uses type-safe constructors instead of raw values
3. **Documentation over assumptions**: Reading actual .purs files in `.spago/` directory was crucial
4. **Spago evolution**: Configuration format changed from snake_case to camelCase

## Next Steps (Future)
- Add force-directed layout animation (currently using backend-provided positions)
- Implement pan/drag functionality
- Add tooltips showing node meanings on hover
- Add edge labels for relationship types
- Optimize rendering for large graphs

## Files Modified
- `frontend/src/Component/EtymologyGraphSimple.purs` (major enhancement)
- `frontend/src/Util/Arabic.purs` (created)
- `frontend/src/Util/Time.purs` (created)
- `frontend/src/Util/RootMatching.purs` (created)
- `frontend/src/Main.purs` (refactored)
- `frontend/spago.yaml` (dependencies updated, format migrated)

## Build Output
```
✓ Build succeeded.
📦 Step 2/2: Bundling with Parcel...
✨ Built in 2.62s
dist/index.js    246.35 kB
✅ Build complete!
```

## Update (2025-10-20 - Late Session)

### UX Improvement: Modal → Inline Panel

**User Feedback**: "when I click anywhere in the overlay it closes. I think better we show it on the same page or hide it there. I fear using this overlay complicates things going forward"

**Change Made**:
- ✅ Removed modal overlay approach (`renderEtymologyModal`)
- ✅ Implemented inline collapsible panel (`renderEtymologyPanel`)
- ✅ Follows same pattern as Analysis and Legend panels
- ✅ Graph appears in main page flow, not as overlay
- ✅ Toggle button (◀/▼) shows/hides panel smoothly

**Implementation Details**:
```purescript
-- Inline panel with collapsible header
renderEtymologyPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderEtymologyPanel state =
  HH.div
    [ -- Panel container with rounded corners and shadow ]
    [ -- Clickable header with toggle button and title
      HH.div [ onClick -> ToggleEtymologyGraph ]
    , -- Conditional content display
      if state.showEtymologyGraph
        then HH.div [ ... graph component ... ]
        else HH.div_ []
    ]
```

**Benefits**:
- No backdrop click issues
- Simpler codebase
- Consistent UX with other panels
- Less complexity for future development

**Build**: ✅ Successful (246 KB bundle, 2 warnings)

**Documentation Updated**:
- `frontend/DESIGN_DECISIONS.md` - Decision rationale
- `CLAUDE.md` - Added UX decision section
- `PHASE4B_TODO.md` - Task tracking


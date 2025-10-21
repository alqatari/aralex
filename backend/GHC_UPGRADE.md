# GHC 9.12.2 Upgrade Guide

## Changes Made

Updated the project to use GHC 9.12.2 (from 9.10.2).

### Files Modified

1. **[cabal.project](cabal.project)**
   ```diff
   - with-compiler: ghc-9.10.2
   + with-compiler: ghc-9.12.2
   ```

2. **[aralex-backend.cabal](aralex-backend.cabal)**

   Updated all `base` library version constraints:
   ```diff
   - base >=4.18 && <5,
   + base >=4.21 && <5,
   ```

   This affects:
   - Library dependencies
   - `aralex-backend` executable
   - `aralex-codegen` executable
   - `aralex-backend-test` test suite

## Next Steps

Before starting a fresh session:

1. **Clean old build artifacts:**
   ```bash
   cd backend
   rm -rf dist-newstyle
   cabal clean
   ```

2. **Update cabal package index:**
   ```bash
   cabal update
   ```

3. **Build with new GHC version:**
   ```bash
   cabal build
   ```

## GHC 9.12.2 Changes

Key changes in GHC 9.12.2 that may affect the project:

- **Base library:** 4.21 (was 4.18 in GHC 9.10.2)
- **Better type inference** in some edge cases
- **Performance improvements** in the compiler
- **Improved error messages**

## Dependency Compatibility

All dependencies specified in the .cabal file should be compatible with GHC 9.12.2:

- ✅ `text >=2.0`
- ✅ `aeson >=2.0`
- ✅ `servant >=0.19`
- ✅ `sqlite-simple >=0.4`
- ✅ `purescript-bridge >=0.15`

These use version ranges that are GHC-agnostic.

## Testing

After the upgrade, verify:

```bash
# Build all components
cabal build all

# Run tests
cabal test

# Build executables
cabal build aralex-backend
cabal build aralex-codegen

# Test in REPL
cabal repl
```

## Rollback (if needed)

If you encounter issues, you can rollback:

```bash
# In cabal.project
with-compiler: ghc-9.10.2

# In aralex-backend.cabal (all instances)
base >=4.18 && <5,
```

Then clean and rebuild.

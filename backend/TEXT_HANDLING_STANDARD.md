# Text Handling Standard for Aralex Backend

## Rule: Always Use `Text` for Unicode Strings

This codebase handles **Arabic text**, which requires proper UTF-8 support. Follow these rules strictly:

## ✅ Correct Text Types

### 1. Use `Text` (from `Data.Text`) for All Unicode Strings
- **Import**: `import Data.Text (Text)`
- **Qualified**: `import qualified Data.Text as T`
- **Use for**: Arabic text, English text, all user-facing strings
- **Why**: Efficient, UTF-8 safe, handles Arabic diacritics correctly

```haskell
-- ✅ CORRECT
import Data.Text (Text)
import qualified Data.Text as T

arabicText :: Text
arabicText = "القرآن الكريم"

processRoot :: Text -> Text
processRoot root = T.toUpper root
```

### 2. Use `ByteString` ONLY for Binary Data
- **Import**: `import Data.ByteString (ByteString)`
- **Use for**: Binary data, network protocols, file I/O when needed
- **Never use for**: Arabic text, user-visible strings

```haskell
-- ✅ CORRECT - Binary data
jsonData :: ByteString
jsonData = encode someData

-- ❌ WRONG - Don't use for text
arabicText :: ByteString  -- NO! Use Text instead
```

### 3. Avoid `String` ([Char])
- **Use ONLY when**: Required by legacy APIs (e.g., `System.FilePath`, `show`)
- **Convert immediately**: Use `T.pack` to convert `String` to `Text`

```haskell
-- ⚠️ ACCEPTABLE - Legacy API requires String
filePath :: FilePath  -- FilePath = String in Haskell
filePath = "/path/to/file"

-- ✅ Convert to Text immediately
fileName :: Text
fileName = T.pack filePath
```

## 🔄 Conversion Rules

### Text ↔ ByteString (UTF-8 encoding)

```haskell
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- Text → ByteString (UTF-8)
textToBytes :: Text -> ByteString
textToBytes = encodeUtf8

-- ByteString → Text (UTF-8)
bytesToText :: ByteString -> Text
bytesToText = decodeUtf8  -- Throws on invalid UTF-8
-- OR
bytesToText = decodeUtf8With lenientDecode  -- Replaces invalid bytes
```

### Text ↔ String

```haskell
import qualified Data.Text as T

-- String → Text
stringToText :: String -> Text
stringToText = T.pack

-- Text → String (avoid if possible)
textToString :: Text -> String
textToString = T.unpack
```

## 📝 Common Patterns

### Database Queries (sqlite-simple)

```haskell
-- ✅ CORRECT - Use Text for Arabic content
instance FromRow DictEntry where
  fromRow = do
    rootText <- field :: RowParser Text        -- ✅ Text
    definitionAr <- field :: RowParser Text    -- ✅ Text for Arabic
    pure $ DictEntry rootText definitionAr

-- Insert with Text
insertEntry :: Connection -> Text -> IO ()
insertEntry conn rootText =
  execute conn "INSERT INTO entries (root) VALUES (?)" (Only rootText)
```

### JSON (Aeson)

```haskell
-- ✅ CORRECT - Aeson works perfectly with Text
data DictEntry = DictEntry
  { root :: Text              -- ✅ Text
  , definitionArabic :: Text  -- ✅ Text for Arabic
  } deriving (Generic, ToJSON, FromJSON)
```

### File Path Handling

```haskell
import System.FilePath ((</>))
import qualified Data.Text as T

-- ✅ CORRECT
loadDictionary :: FilePath -> Text -> IO ()
loadDictionary dataDir filename = do
  let path = dataDir </> T.unpack filename  -- FilePath requires String
  content <- readFile path                  -- Read as Text
  ...
```

## ❌ Common Mistakes

### Mistake 1: Using String for Arabic Text
```haskell
-- ❌ WRONG
arabicWord :: String
arabicWord = "كلمة"  -- Inefficient, potential encoding issues

-- ✅ CORRECT
arabicWord :: Text
arabicWord = "كلمة"  -- Efficient, UTF-8 safe
```

### Mistake 2: ByteString for Text
```haskell
-- ❌ WRONG
rootText :: ByteString
rootText = "جذر"  -- This is text, not binary data!

-- ✅ CORRECT
rootText :: Text
rootText = "جذر"
```

### Mistake 3: Not Using Qualified Imports
```haskell
-- ❌ CONFUSING
import Data.Text
import Data.ByteString

text1 = pack "hello"  -- Which pack? Text or ByteString?

-- ✅ CORRECT
import qualified Data.Text as T
import qualified Data.ByteString as BS

text1 = T.pack "hello"    -- Clear!
bytes1 = BS.pack [65, 66]  -- Clear!
```

## 🎯 Summary

| Type | Use For | Import |
|------|---------|--------|
| `Text` | All Unicode strings (Arabic, English) | `Data.Text` |
| `ByteString` | Binary data only | `Data.ByteString` |
| `String` | Legacy APIs only (convert ASAP) | Prelude |

## 🔍 Checking Your Code

Before committing, verify:
- ✅ All Arabic/English text uses `Text`
- ✅ `ByteString` used only for binary data
- ✅ UTF-8 encoding/decoding uses `Data.Text.Encoding`
- ✅ `String` usage minimized (only for file paths, `show`)

---

**When in doubt**: Use `Text` from `Data.Text`!

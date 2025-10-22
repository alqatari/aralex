{-|
Module      : API.Handlers
Description : API request handlers
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Handler implementations for API endpoints with database queries.
-}

module API.Handlers
  ( lookupRoot
  , lookupMorphology
  , lookupVerse
  , lookupVersesByRoot
  , performAnalysis
  , getAllLetters
  , getLetterMeaning
  , DatabaseConnections(..)
  ) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

import Database.Instances ()  -- Import FromRow instances
import Database.Schema (DatabaseConnections(..))
import Domain.Dictionary (DictEntry(..), RootText(..), DictionaryId, normalizeRoot)
import Domain.Morphology (QuranicWord, MorphSegment, aggregateSegments)
import Domain.MorphologyDTO (QuranicWordDTO, wordToDTO, segmentFromDTO)
import Domain.Verse (VerseText(..), VerseRef(..), mkVerseRef, VerseWithRoot(..), getSurahInfo, SurahInfo(..), SurahName(..))
import Domain.Phonosemantics
  ( WordAnalysis(..)
  , ArabicLetter(..)
  , LetterBreakdown(..)
  , LetterMeaning(..)
  , LetterAttribute(..)
  , LetterCategory(..)
  , ArticulationPoint(..)
  , PhonosemanticPattern(..)
  , DictionaryEvidence(..)
  , QuranicEvidence(..)
  , extractLetters
  , getLetterCategory
  , getArticulationPoint
  )

-- | Normalize letter for database lookup
-- Converts all hamza forms to standalone ء
normalizeLetter :: Text -> Text
normalizeLetter t = case T.unpack t of
  ['أ'] -> "ء"  -- hamza on alif
  ['إ'] -> "ء"  -- hamza under alif
  ['آ'] -> "ء"  -- hamza madda
  ['ؤ'] -> "ء"  -- hamza on waw
  ['ئ'] -> "ء"  -- hamza on ya
  _     -> t    -- keep other letters as-is

-- | Look up dictionary entries by root
lookupRoot :: Connection -> RootText -> IO [DictEntry]
lookupRoot conn (RootText rootText) = do
  query conn
    "SELECT e.id, e.dict_source_id, e.root, e.root_normalized, e.definition_arabic, \
    \(CASE s.author \
      \WHEN 'Al-Khalīl b. Aḥmad al-Farāhīdī' THEN 'الخليل بن أحمد الفراهيدي، كتاب العين (ت. ' || s.death_year || ' م)' \
      \WHEN 'Al-Jawharī' THEN 'الجوهري، الصحاح (ت. ' || s.death_year || ' م)' \
      \WHEN 'Ibn Fāris' THEN 'ابن فارس، مقاييس اللغة (ت. ' || s.death_year || ' م)' \
      \WHEN 'Ibn Sīda' THEN 'ابن سيده، المحكم (ت. ' || s.death_year || ' م)' \
      \WHEN 'Al-Rāghib al-Iṣfahānī' THEN 'الراغب الأصفهاني، المفردات (ت. ' || s.death_year || ' م)' \
      \WHEN 'Ibn Manẓūr' THEN 'ابن منظور، لسان العرب (ت. ' || s.death_year || ' م)' \
      \WHEN 'Fīrūzābādī' THEN 'الفيروزآبادي، القاموس المحيط (ت. ' || s.death_year || ' م)' \
      \ELSE s.author || ', ' || s.title || ' (d. ' || s.death_year || ' CE)' \
    \END) as dictionary_source \
    \FROM dictionary_entries e \
    \JOIN dictionary_sources s ON e.dict_source_id = s.id \
    \WHERE e.root_normalized = ? \
    \GROUP BY e.dict_source_id, e.definition_arabic \
    \ORDER BY s.death_year ASC \
    \LIMIT 100"
    (Only rootText)

-- | Look up morphology by verse
lookupMorphology :: Connection -> Int -> Int -> IO [QuranicWordDTO]
lookupMorphology conn surahNum verseNum = do
  rows <- query conn
    "SELECT surface_form, root, lemma, pos, segments_json FROM quranic_words WHERE surah = ? AND verse = ? ORDER BY word_position"
    (surahNum, verseNum) :: IO [(Text, Maybe Text, Maybe Text, String, BL.ByteString)]

  -- Convert to DTOs
  pure $ map rowToDTO rows
  where
    rowToDTO (_surface, _root, _lemma, _pos, segmentsJson) =
      -- Parse segments JSON and reconstruct word
      case decode segmentsJson of
        Just segmentDTOs ->
          let segs = map segmentFromDTO segmentDTOs
          in case aggregateSegments segs of
               (qWord:_) -> wordToDTO qWord
               [] -> error "aggregateSegments returned empty list (should not happen)"
        Nothing -> error "Failed to decode segments JSON"

-- | Look up verse by reference
lookupVerse :: Connection -> Int -> Int -> IO (Maybe VerseText)
lookupVerse conn surahNum verseNum = do
  results <- query conn
    "SELECT * FROM verses WHERE surah = ? AND verse = ?"
    (surahNum, verseNum) :: IO [VerseText]
  pure $ case results of
    (v:_) -> Just v
    []    -> Nothing

-- | Look up all verses containing a specific root
lookupVersesByRoot :: Connection -> RootText -> IO [VerseWithRoot]
lookupVersesByRoot conn (RootText rootText) = do
  -- Query to find all verses containing words with this root
  -- Group by verse to get word positions and occurrences
  rows <- query conn
    "SELECT v.surah, v.verse, v.text, GROUP_CONCAT(w.word_position) as positions, COUNT(w.id) as occurrences \
    \FROM quranic_words w \
    \JOIN verses v ON w.surah = v.surah AND w.verse = v.verse \
    \WHERE w.root = ? \
    \GROUP BY v.surah, v.verse \
    \ORDER BY v.surah ASC, v.verse ASC"
    (Only rootText) :: IO [(Int, Int, Text, Text, Int)]

  -- Convert rows to VerseWithRoot
  pure $ map rowToVerseWithRoot rows
  where
    rowToVerseWithRoot :: (Int, Int, Text, Text, Int) -> VerseWithRoot
    rowToVerseWithRoot (surahNum, verseNum, verseText, positionsStr, occurrences) =
      let ref = case mkVerseRef surahNum verseNum of
                  Just r -> r
                  Nothing -> error $ "Invalid verse reference: " <> show surahNum <> ":" <> show verseNum
          -- Parse comma-separated positions (e.g., "1,5,10")
          positions = map (read . T.unpack) $ T.split (== ',') positionsStr
          -- Get surah name from metadata
          surahNameArabic = case getSurahInfo surahNum of
                        Just info -> arabic (surahName info)
                        Nothing -> ""
      in VerseWithRoot
          { vwrVerseRef = ref
          , vwrText = verseText
          , vwrRoot = rootText
          , vwrSurahName = surahNameArabic
          , vwrWordIndices = positions
          , vwrOccurrences = occurrences
          }

-- | Perform phonosemantic analysis using Hassan Abbas's theory
performAnalysis :: DatabaseConnections -> Text -> IO WordAnalysis
performAnalysis (DatabaseConnections dictConn quranConn) word = do
  -- 1. Extract letters from word (remove diacritics)
  let letters = extractLetters word

  -- 2. Try to find root from morphology (uses aralex.db)
  rootMaybe <- findWordRoot quranConn word

  -- 3. Build letter breakdown with meanings from database (uses aralex.db)
  letterBreakdowns <- mapM (buildLetterBreakdown quranConn) (zip [1..] letters)

  -- 4. Detect phonosemantic patterns
  let patterns = detectPatterns letters letterBreakdowns

  -- 5. Gather Quranic evidence if we have a root
  quranEvid <- case rootMaybe of
    Just root -> gatherQuranicEvidence quranConn root letters
    Nothing -> pure []

  -- 6. Synthesize composite meaning from letter meanings
  let compositeMeaning' = synthesizeMeaning letterBreakdowns patterns

  pure $ WordAnalysis
    { analyzedWord = word
    , analysisRoot = rootMaybe
    , letterBreakdown = letterBreakdowns
    , patterns = patterns
    , compositeMeaning = compositeMeaning'
    , dictEvidence = []
    , quranicEvidence = quranEvid
    , analysisConfidence = 1.0
    }

-- | Try to find the root of a word by querying morphology
-- Normalizes the input word (removes diacritics) and searches by root
findWordRoot :: Connection -> Text -> IO (Maybe RootText)
findWordRoot conn word = do
  let RootText normalizedWord = normalizeRoot (RootText word)
  results <- query conn
    "SELECT DISTINCT root FROM quranic_words WHERE root = ? AND root IS NOT NULL LIMIT 1"
    (Only normalizedWord) :: IO [Only (Maybe Text)]
  case results of
    (Only (Just r):_) -> pure $ Just (RootText r)
    _ -> pure Nothing

-- | Build letter breakdown with meaning from database
buildLetterBreakdown :: Connection -> (Int, ArabicLetter) -> IO LetterBreakdown
buildLetterBreakdown conn (pos, letter@(ArabicLetter letterText)) = do
  -- Normalize letter for database lookup (all hamza forms → ء)
  let normalizedLetter = normalizeLetter letterText

  -- Query letter meaning from database
  meaningResult <- query conn
    "SELECT primary_meaning_ar, primary_meaning_en, semantic_domains, examples FROM letter_meanings WHERE letter = ?"
    (Only normalizedLetter) :: IO [(Text, Maybe Text, Text, Text)]

  let meaning = case meaningResult of
        ((primaryAr, primaryEn, _domains, examplesJson):_) -> Just $ LetterMeaning
          { letter = letter
          , primaryMeaning = primaryAr
          , semanticDomains = []  -- TODO: parse JSON domains
          , arabicDesc = maybe primaryAr id primaryEn
          , examples = []  -- TODO: parse JSON examples
          }
        [] -> Nothing

  pure $ LetterBreakdown
    { position = pos
    , letterChar = letter
    , category = getLetterCategory letter
    , articulation = getArticulationPoint letter
    , attributes = getLetterAttributes letter
    , meaning = meaning
    }

-- | Get phonetic attributes for a letter
getLetterAttributes :: ArabicLetter -> [LetterAttribute]
getLetterAttributes (ArabicLetter t) = case T.unpack t of
  [c] -> case c of
    'ب' -> [Voiced, Plosive]
    'ت' -> [Voiceless, Plosive]
    'ث' -> [Voiceless, Fricative]
    'ج' -> [Voiced, Plosive]
    'ح' -> [Voiceless, Fricative]
    'خ' -> [Voiceless, Fricative]
    'د' -> [Voiced, Plosive]
    'ذ' -> [Voiced, Fricative]
    'ر' -> [Voiced, Approximant]
    'ز' -> [Voiced, Fricative]
    'س' -> [Voiceless, Fricative]
    'ش' -> [Voiceless, Fricative]
    'ص' -> [Voiceless, Emphatic, Fricative]
    'ض' -> [Voiced, Emphatic, Plosive]
    'ط' -> [Voiceless, Emphatic, Plosive]
    'ظ' -> [Voiced, Emphatic, Fricative]
    'ع' -> [Voiced, Fricative]
    'غ' -> [Voiced, Fricative]
    'ف' -> [Voiceless, Fricative]
    'ق' -> [Voiceless, Plosive]
    'ك' -> [Voiceless, Plosive]
    'ل' -> [Voiced, Approximant]
    'م' -> [Voiced, Nasal]
    'ن' -> [Voiced, Nasal]
    'ه' -> [Voiceless, Fricative]
    'و' -> [Voiced, Approximant]
    'ي' -> [Voiced, Approximant]
    'ء' -> [Voiceless, Plosive]
    _ -> []
  _ -> []

-- | Detect phonosemantic patterns in letters
detectPatterns :: [ArabicLetter] -> [LetterBreakdown] -> [PhonosemanticPattern]
detectPatterns letters breakdowns =
  detectEmphatics letters ++
  detectGutturals letters ++
  detectLabials letters ++
  detectMovement letters ++
  detectOpening letters

-- | Detect emphatic consonant pattern (intensification)
detectEmphatics :: [ArabicLetter] -> [PhonosemanticPattern]
detectEmphatics letters =
  let emphaticLetters = filter isEmphatic letters
  in if not (null emphaticLetters)
     then [PhonosemanticPattern
            { patternName = "intensification"
            , letterSequence = emphaticLetters
            , semanticEffect = "The emphatic consonants (ط، ض، ص، ظ) add intensity and strength to the word's meaning"
            , confidence = 0.9
            }]
     else []
  where
    isEmphatic (ArabicLetter t) = t `elem` ["ط", "ض", "ص", "ظ"]

-- | Detect guttural pattern (depth/spirituality)
detectGutturals :: [ArabicLetter] -> [PhonosemanticPattern]
detectGutturals letters =
  let gutturalLetters = filter isGuttural letters
  in if length gutturalLetters >= 2
     then [PhonosemanticPattern
            { patternName = "depth"
            , letterSequence = gutturalLetters
            , semanticEffect = "Multiple guttural letters (ء، ه، ع، ح، غ، خ) suggest depth, profundity, or metaphysical concepts"
            , confidence = 0.8
            }]
     else []
  where
    isGuttural (ArabicLetter t) = t `elem` ["ء", "ه", "ع", "ح", "غ", "خ"]

-- | Detect labial pattern (containment)
detectLabials :: [ArabicLetter] -> [PhonosemanticPattern]
detectLabials letters =
  let labialLetters = filter isLabial letters
  in if length labialLetters >= 2
     then [PhonosemanticPattern
            { patternName = "containment"
            , letterSequence = labialLetters
            , semanticEffect = "Labial letters (ب، م، و، ف) suggest enclosure, gathering, or containment"
            , confidence = 0.75
            }]
     else []
  where
    isLabial (ArabicLetter t) = t `elem` ["ب", "م", "و", "ف"]

-- | Detect movement pattern (ر presence)
detectMovement :: [ArabicLetter] -> [PhonosemanticPattern]
detectMovement letters =
  if any isRa letters
  then [PhonosemanticPattern
         { patternName = "movement"
         , letterSequence = filter isRa letters
         , semanticEffect = "The letter ر indicates vibration, repetition, or movement"
         , confidence = 0.85
         }]
  else []
  where
    isRa (ArabicLetter t) = t == "ر"

-- | Detect opening pattern (ف presence)
detectOpening :: [ArabicLetter] -> [PhonosemanticPattern]
detectOpening letters =
  if any isFa letters
  then [PhonosemanticPattern
         { patternName = "opening"
         , letterSequence = filter isFa letters
         , semanticEffect = "The letter ف indicates opening, separation, or revelation"
         , confidence = 0.85
         }]
  else []
  where
    isFa (ArabicLetter t) = t == "ف"

-- | Gather Quranic evidence for phonosemantic analysis
gatherQuranicEvidence :: Connection -> RootText -> [ArabicLetter] -> IO [QuranicEvidence]
gatherQuranicEvidence conn (RootText rootText) letters = do
  -- Query verses using this root
  verses <- query conn
    "SELECT v.surah, v.verse, v.text \
    \FROM quranic_words w \
    \JOIN verses v ON w.surah = v.surah AND w.verse = v.verse \
    \WHERE w.root = ? \
    \LIMIT 3"
    (Only rootText) :: IO [(Int, Int, Text)]

  pure $ map (\(surah, verse, text) ->
    case mkVerseRef surah verse of
      Just ref -> QuranicEvidence
        { quranicVerseRef = ref
        , wordContext = T.take 100 text  -- First 100 chars
        , semanticContext = "Quranic usage of root " <> rootText
        , quranicLetterSupport = letters
        }
      Nothing -> QuranicEvidence
        { quranicVerseRef = VerseRef 1 1  -- Fallback
        , wordContext = text
        , semanticContext = "Invalid verse reference"
        , quranicLetterSupport = []
        }
    ) verses

-- | Synthesize composite meaning from letter breakdowns and patterns
synthesizeMeaning :: [LetterBreakdown] -> [PhonosemanticPattern] -> Text
synthesizeMeaning breakdowns _patterns =
  let letterMeanings = [primaryMeaning m | Just m <- map meaning breakdowns]
      combined = if null letterMeanings
                 then "لا توجد معاني للحروف"
                 else T.intercalate "، " letterMeanings
  in combined

-- | Get all letter meanings
getAllLetters :: Connection -> IO [(Text, Text, Text, Text)]
getAllLetters conn = do
  query_ conn
    "SELECT letter, letter_name_ar, primary_meaning_ar, primary_meaning_en FROM letter_meanings ORDER BY id"

-- | Get meaning for a specific letter
getLetterMeaning :: Connection -> Text -> IO (Maybe (Text, Text, Text, Text, Text, Text, Text, Text, Text, Text))
getLetterMeaning conn letterChar = do
  results <- query conn
    "SELECT letter, letter_name_ar, category, articulation_point, primary_meaning_ar, primary_meaning_en, semantic_domains, attributes, examples, source_reference FROM letter_meanings WHERE letter = ?"
    (Only letterChar) :: IO [(Text, Text, Text, Text, Text, Maybe Text, Text, Text, Text, Text)]
  case results of
    ((l, ln, c, ap, pma, pme, sd, a, e, sr):_) ->
      pure $ Just (l, ln, c, ap, pma, maybe "" id pme, sd, a, e, sr)
    [] -> pure Nothing

{-|
Module      : Domain.Phonosemantics
Description : Domain types for phonosemantic analysis of Arabic letters
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Types for analyzing Arabic words through letter-level phonosemantics
based on Hassan Abbas's theory (خصائص الحروف العربية ومعانيها).
-}

module Domain.Phonosemantics
  ( -- * Arabic Letters
    ArabicLetter(..)
  , LetterCategory(..)
  , ArticulationPoint(..)
  , LetterAttribute(..)
    -- * Letter Meanings
  , LetterMeaning(..)
  , SemanticDomain(..)
    -- * Word Analysis
  , LetterBreakdown(..)
  , WordAnalysis(..)
  , PhonosemanticPattern(..)
    -- * Cross-References
  , DictionaryEvidence(..)
  , QuranicEvidence(..)
    -- * Utilities
  , getLetterCategory
  , getArticulationPoint
  , extractLetters
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Domain.Dictionary (RootText(..))
import Domain.Verse (VerseRef)

-- | Arabic letter (represented as single-character Text for JSON serialization)
-- Note: While semantically a single character, we use Text instead of Char because:
-- 1. Haskell Char is Unicode code point (full Arabic support)
-- 2. PureScript Char is UTF-16 code unit (limited)
-- 3. Text/String works seamlessly with argonaut JSON encoding
newtype ArabicLetter = ArabicLetter { unLetter :: Text }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ArabicLetter
instance FromJSON ArabicLetter

-- | Letter categories based on phonetic properties
data LetterCategory
  = Labial        -- شفوية (ب، م، و، ف)
  | Dental        -- أسنانية (ث، ذ، ظ)
  | Alveolar      -- لثوية (ت، د، ط، ض، ص، ز، س، ن، ل، ر)
  | Palatal       -- غارية (ج، ش، ي)
  | Velar         -- طبقية (ك)
  | Uvular        -- لهوية (ق)
  | Pharyngeal    -- حلقية (ح، ع)
  | Glottal       -- حنجرية (ء، ه)
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON LetterCategory
instance FromJSON LetterCategory

-- | Articulation point (مخرج الحرف)
data ArticulationPoint
  = Lips              -- الشفتان
  | LipsTeeth         -- الشفتان والأسنان
  | TongueTips        -- طرف اللسان
  | TongueTeeth       -- اللسان والأسنان
  | TongueGums        -- اللسان واللثة
  | TongueHardPalate  -- اللسان والحنك الصلب
  | TongueSoftPalate  -- اللسان والحنك اللين
  | Uvula             -- اللهاة
  | Pharynx           -- الحلق
  | Larynx            -- الحنجرة
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON ArticulationPoint
instance FromJSON ArticulationPoint

-- | Letter phonetic attributes
data LetterAttribute
  = Voiced         -- مجهور
  | Voiceless      -- مهموس
  | Emphatic       -- مفخم
  | NonEmphatic    -- مرقق
  | Nasal          -- أنفي
  | Fricative      -- احتكاكي
  | Plosive        -- انفجاري
  | Approximant    -- تقريبي
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON LetterAttribute
instance FromJSON LetterAttribute

-- | Semantic domain for letter meanings
data SemanticDomain
  = Motion            -- حركة
  | Containment       -- احتواء
  | Separation        -- انفصال
  | Strength          -- قوة
  | Weakness          -- ضعف
  | Clarity           -- وضوح
  | Obscurity         -- غموض
  | Expansion         -- اتساع
  | Compression       -- انضغاط
  | Elevation         -- علو
  | Depression        -- انخفاض
  | Connection        -- اتصال
  | Disruption        -- انقطاع
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON SemanticDomain
instance FromJSON SemanticDomain

-- | Letter meaning from Hassan Abbas's theory
data LetterMeaning = LetterMeaning
  { letter          :: ArabicLetter
  , primaryMeaning  :: Text              -- ^ Core semantic value
  , semanticDomains :: [SemanticDomain]  -- ^ Associated domains
  , arabicDesc      :: Text              -- ^ Description in Arabic
  , examples        :: [Text]            -- ^ Example words demonstrating meaning
  } deriving (Eq, Show, Generic)

instance ToJSON LetterMeaning
instance FromJSON LetterMeaning

-- | Individual letter in word breakdown
data LetterBreakdown = LetterBreakdown
  { position        :: !Int              -- ^ Position in word (1-based)
  , letterChar      :: ArabicLetter
  , category        :: LetterCategory
  , articulation    :: ArticulationPoint
  , attributes      :: [LetterAttribute]
  , meaning         :: Maybe LetterMeaning
  } deriving (Eq, Show, Generic)

instance ToJSON LetterBreakdown
instance FromJSON LetterBreakdown

-- | Pattern across letters in a word
data PhonosemanticPattern = PhonosemanticPattern
  { patternName     :: Text              -- ^ e.g., "intensification", "movement"
  , letterSequence  :: [ArabicLetter]    -- ^ Letters forming pattern
  , semanticEffect  :: Text              -- ^ How pattern affects meaning
  , confidence      :: Double            -- ^ 0.0-1.0 confidence score
  } deriving (Eq, Show, Generic)

instance ToJSON PhonosemanticPattern
instance FromJSON PhonosemanticPattern

-- | Evidence from dictionary definitions
data DictionaryEvidence = DictionaryEvidence
  { dictRoot            :: RootText
  , dictName            :: Text
  , relevantExcerpt     :: Text              -- ^ Excerpt supporting phonosemantic analysis
  , dictLetterSupport   :: [ArabicLetter]    -- ^ Letters this evidence supports
  } deriving (Eq, Show, Generic)

instance ToJSON DictionaryEvidence
instance FromJSON DictionaryEvidence

-- | Evidence from Quranic usage
data QuranicEvidence = QuranicEvidence
  { quranicVerseRef     :: VerseRef
  , wordContext         :: Text              -- ^ The word in context
  , semanticContext     :: Text              -- ^ Meaning in this verse
  , quranicLetterSupport :: [ArabicLetter]   -- ^ Letters this evidence supports
  } deriving (Eq, Show, Generic)

instance ToJSON QuranicEvidence
instance FromJSON QuranicEvidence

-- | Complete phonosemantic analysis of a word
data WordAnalysis = WordAnalysis
  { analyzedWord       :: Text
  , analysisRoot       :: Maybe RootText
  , letterBreakdown    :: [LetterBreakdown]
  , patterns           :: [PhonosemanticPattern]
  , compositeMeaning   :: Text              -- ^ Synthesized meaning from letters
  , dictEvidence       :: [DictionaryEvidence]
  , quranicEvidence    :: [QuranicEvidence]
  , analysisConfidence :: Double            -- ^ Overall confidence (0.0-1.0)
  } deriving (Eq, Show, Generic)

instance ToJSON WordAnalysis
instance FromJSON WordAnalysis

-- | Get letter category based on phonetic properties
getLetterCategory :: ArabicLetter -> LetterCategory
getLetterCategory (ArabicLetter t) = case T.unpack t of
  [c] -> case c of
    'ب' -> Labial
    'م' -> Labial
    'و' -> Labial
    'ف' -> Labial
    'ث' -> Dental
    'ذ' -> Dental
    'ظ' -> Dental
    'ت' -> Alveolar
    'د' -> Alveolar
    'ط' -> Alveolar
    'ض' -> Alveolar
    'ص' -> Alveolar
    'ز' -> Alveolar
    'س' -> Alveolar
    'ن' -> Alveolar
    'ل' -> Alveolar
    'ر' -> Alveolar
    'ج' -> Palatal
    'ش' -> Palatal
    'ي' -> Palatal
    'ك' -> Velar
    'ق' -> Uvular
    'ح' -> Pharyngeal
    'ع' -> Pharyngeal
    'ء' -> Glottal
    'ه' -> Glottal
    _   -> Glottal  -- Default
  [] -> Glottal   -- Empty string fallback
  _  -> Glottal   -- Multi-character fallback

-- | Get articulation point for letter
getArticulationPoint :: ArabicLetter -> ArticulationPoint
getArticulationPoint (ArabicLetter t) = case T.unpack t of
  [c] -> case c of
    'ب' -> Lips
    'م' -> Lips
    'و' -> Lips
    'ف' -> LipsTeeth
    'ت' -> TongueTips
    'د' -> TongueTips
    'ط' -> TongueTips
    'ث' -> TongueTeeth
    'ذ' -> TongueTeeth
    'ظ' -> TongueTeeth
    'ص' -> TongueGums
    'ز' -> TongueGums
    'س' -> TongueGums
    'ن' -> TongueGums
    'ل' -> TongueGums
    'ر' -> TongueGums
    'ج' -> TongueHardPalate
    'ش' -> TongueHardPalate
    'ي' -> TongueHardPalate
    'ك' -> TongueSoftPalate
    'ق' -> Uvula
    'ح' -> Pharynx
    'ع' -> Pharynx
    'ء' -> Larynx
    'ه' -> Larynx
    _   -> Larynx  -- Default
  [] -> Larynx   -- Empty string fallback
  _  -> Larynx   -- Multi-character fallback

-- | Extract Arabic letters from text (filtering diacritics)
extractLetters :: Text -> [ArabicLetter]
extractLetters = map (ArabicLetter . T.singleton) . filter isArabicLetter . T.unpack
  where
    isArabicLetter c =
      (c >= '\x0621' && c <= '\x064A')  -- Arabic letters range
      && not (c >= '\x064B' && c <= '\x0652')  -- Exclude diacritics

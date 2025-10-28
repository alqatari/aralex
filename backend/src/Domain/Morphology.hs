{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      : Domain.Morphology
-- Description : Domain types for Quranic morphological analysis
-- Copyright   : (c) Ali Al-Qatari, 2025
-- License     : MIT
--
-- GADTs and type families for type-safe morphological features.
-- Based on Mustafa's improved Quranic Corpus (130,030 morpheme segments).
module Domain.Morphology
  ( -- * Location
    MorphLocation (..),
    mkLocation,
    compareLocation,

    -- * Part of Speech
    PartOfSpeech (..),
    POSTag (..),

    -- * Morphological Features (GADT)
    MorphFeature (..),
    VerbForm (..),
    VerbMood (..),
    PersonGenderNumber (..),
    NounCase (..),
    NounNumber (..),
    NounGender (..),
    Definiteness (..),
    ParticleFamily (..),
    Voice (..),

    -- * Morpheme Segment
    MorphSegment (..),
    isPrefix,
    isSuffix,
    isStem,
    getRoot,
    getLemma,

    -- * Word Aggregation
    WordLocation (..),
    QuranicWord (..),
    aggregateSegments,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Location of a morpheme segment: surah:verse:word:segment
data MorphLocation = MorphLocation
  { -- | Surah number (1-114)
    surah :: !Int,
    -- | Verse number (1-286)
    verse :: !Int,
    -- | Word position in verse
    word :: !Int,
    -- | Segment position in word
    segment :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MorphLocation

instance FromJSON MorphLocation

-- | Smart constructor with validation
mkLocation :: Int -> Int -> Int -> Int -> Maybe MorphLocation
mkLocation s v w seg
  | s < 1 || s > 114 = Nothing
  | v < 1 = Nothing
  | w < 1 = Nothing
  | seg < 1 = Nothing
  | otherwise = Just $ MorphLocation s v w seg

-- | Compare locations for ordering
compareLocation :: MorphLocation -> MorphLocation -> Ordering
compareLocation l1 l2 =
  compare
    (surah l1, verse l1, word l1, segment l1)
    (surah l2, verse l2, word l2, segment l2)

-- | Word location (aggregated segments)
data WordLocation = WordLocation
  { wSurah :: !Int,
    wVerse :: !Int,
    wWord :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON WordLocation

instance FromJSON WordLocation

-- | Part-of-speech tag from corpus
data POSTag = N | V | P -- Noun, Verb, Particle
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON POSTag

instance FromJSON POSTag

-- | High-level part of speech classification
data PartOfSpeech
  = Noun
  | Verb
  | Particle
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PartOfSpeech

instance FromJSON PartOfSpeech

-- | Verbal forms (I-X for triliteral roots)
data VerbForm
  = Form1 -- فَعَلَ
  | Form2 -- فَعَّلَ
  | Form3 -- فَاعَلَ
  | Form4 -- أَفْعَلَ
  | Form5 -- تَفَعَّلَ
  | Form6 -- تَفَاعَلَ
  | Form7 -- انْفَعَلَ
  | Form8 -- افْتَعَلَ
  | Form9 -- افْعَلَّ
  | Form10 -- اسْتَفْعَلَ
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON VerbForm

instance FromJSON VerbForm

-- | Verbal mood
data VerbMood
  = Indicative -- مرفوع (MOOD:IND)
  | Subjunctive -- منصوب (MOOD:SUBJ)
  | Jussive -- مجزوم (MOOD:JUS)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON VerbMood

instance FromJSON VerbMood

-- | Person, gender, and number combined
data PersonGenderNumber
  = FirstPersonSingular -- 1S
  | FirstPersonPlural -- 1P
  | SecondPersonMaleSingular -- 2MS
  | SecondPersonFemaleSingular -- 2FS
  | SecondPersonMaleDual -- 2MD
  | SecondPersonFemaleDual -- 2FD
  | SecondPersonMalePlural -- 2MP
  | SecondPersonFemalePlural -- 2FP
  | ThirdPersonMaleSingular -- 3MS
  | ThirdPersonFemaleSingular -- 3FS
  | ThirdPersonMaleDual -- 3MD
  | ThirdPersonFemaleDual -- 3FD
  | ThirdPersonMalePlural -- 3MP
  | ThirdPersonFemalePlural -- 3FP
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PersonGenderNumber

instance FromJSON PersonGenderNumber

-- | Noun case
data NounCase
  = Nominative -- مرفوع (NOM)
  | Accusative -- منصوب (ACC)
  | Genitive -- مجرور (GEN)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NounCase

instance FromJSON NounCase

-- | Noun number
data NounNumber
  = Singular -- (S)
  | Dual -- (D)
  | Plural -- (P)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NounNumber

instance FromJSON NounNumber

-- | Noun gender
data NounGender = Masculine | Feminine
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NounGender

instance FromJSON NounGender

-- | Definiteness
data Definiteness = Definite | Indefinite
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Definiteness

instance FromJSON Definiteness

-- | Special particle families (3,879 instances in corpus)
data ParticleFamily
  = InnFamily -- إن وأخواتها (nasb particles)
  | KanaFamily -- كان وأخواتها (raf' + nasb verbs)
  | LawFamily -- لو وأخواتها (conditional particles)
  | ZannFamily -- ظن وأخواتها (cognitive verbs)
  | RajaaFamily -- رجا وأخواتها (hope/fear verbs)
  | OtherFamily Text -- Other families
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ParticleFamily

instance FromJSON ParticleFamily

-- | Voice (active/passive)
data Voice = Active | Passive
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Voice

instance FromJSON Voice

-- | Morphological features as GADT
-- Provides compile-time guarantees about feature combinations
data MorphFeature where
  -- Universal features
  Root :: Text -> MorphFeature
  Lemma :: Text -> MorphFeature
  Prefix :: MorphFeature
  Suffix :: MorphFeature
  -- Verb features
  Perfect :: MorphFeature
  Imperfect :: MorphFeature
  Imperative :: MorphFeature
  VerbFormF :: VerbForm -> MorphFeature
  Mood :: VerbMood -> MorphFeature
  Voice :: Voice -> MorphFeature -- NEW: Active/Passive
  ActiveParticiple :: MorphFeature
  PassiveParticiple :: MorphFeature
  -- Noun features
  ProperNoun :: MorphFeature
  Pronoun :: MorphFeature
  Case :: NounCase -> MorphFeature
  Number :: NounNumber -> MorphFeature
  Gender :: NounGender -> MorphFeature
  Definiteness :: Definiteness -> MorphFeature
  Adjective :: MorphFeature
  -- Particle features
  Determiner :: MorphFeature -- ال
  Conjunction :: MorphFeature -- و، ف
  Negation :: MorphFeature -- لا
  Preposition :: MorphFeature
  RelativePronoun :: MorphFeature
  Exceptive :: MorphFeature
  ParticleFamily :: ParticleFamily -> MorphFeature -- NEW: إن وأخواتها etc.

  -- Special markers
  Emphasis :: MorphFeature -- EMPH (لام التوكيد)
  Vocative :: MorphFeature -- VOC (حرف النداء)
  Interrogative :: MorphFeature -- INTG (حرف الاستفهام)
  Answer :: MorphFeature -- ANS (حرف الجواب)
  Restriction :: MorphFeature -- REM (حرف الحصر)
  Circumstantial :: MorphFeature -- CIRC (حال)
  Purpose :: MorphFeature -- PRP (لام التعليل)
  Future :: MorphFeature -- FUT (سين/سوف)
  Prohibition :: MorphFeature -- PROH (لا الناهية)
  Resumption :: MorphFeature -- RSLT (فاء الاستئناف)
  Cause :: MorphFeature -- CAUS (لام السببية)
  Retraction :: MorphFeature -- RET (الاستدراك)
  Inchoative :: MorphFeature -- INC (حرف الابتداء)
  Surprise :: MorphFeature -- SUP (حرف المفاجأة)
  Preventive :: MorphFeature -- PREV (حرف الكف)
  Amplification :: MorphFeature -- AMD (التوكيد بالحروف الزائدة)
  Interpretation :: MorphFeature -- EXL (حرف التفسير)
  Certainty :: MorphFeature -- CERT (لام التوكيد)
  Comitative :: MorphFeature -- COM (واو المعية)
  Supplemental :: MorphFeature -- SUP (حرف الزيادة)
  Equalization :: MorphFeature -- EQ (حرف التسوية)
  Conditional :: MorphFeature -- COND (أداة الشرط)
  Result :: MorphFeature -- RSLT (فاء الجزاء)
  Location :: MorphFeature -- LOC (ظرف المكان)
  Time :: MorphFeature -- T (ظرف الزمان)

  -- Person/Gender/Number
  PGN :: PersonGenderNumber -> MorphFeature

deriving instance Eq MorphFeature

deriving instance Show MorphFeature

instance ToJSON MorphFeature where
  toJSON (Root t) = A.object ["type" .= ("root" :: Text), "value" .= t]
  toJSON (Lemma t) = A.object ["type" .= ("lemma" :: Text), "value" .= t]
  toJSON Prefix = A.String "prefix"
  toJSON Suffix = A.String "suffix"
  toJSON Perfect = A.String "perfect"
  toJSON Imperfect = A.String "imperfect"
  toJSON Imperative = A.String "imperative"
  toJSON (VerbFormF vf) = A.object ["type" .= ("verb_form" :: Text), "value" .= vf]
  toJSON (Mood m) = A.object ["type" .= ("mood" :: Text), "value" .= m]
  toJSON (Voice v) = A.object ["type" .= ("voice" :: Text), "value" .= v]
  toJSON ActiveParticiple = A.String "active_participle"
  toJSON PassiveParticiple = A.String "passive_participle"
  toJSON ProperNoun = A.String "proper_noun"
  toJSON Pronoun = A.String "pronoun"
  toJSON (Case c) = A.object ["type" .= ("case" :: Text), "value" .= c]
  toJSON (Number n) = A.object ["type" .= ("number" :: Text), "value" .= n]
  toJSON (Gender g) = A.object ["type" .= ("gender" :: Text), "value" .= g]
  toJSON (Definiteness d) = A.object ["type" .= ("definiteness" :: Text), "value" .= d]
  toJSON Adjective = A.String "adjective"
  toJSON Determiner = A.String "determiner"
  toJSON Conjunction = A.String "conjunction"
  toJSON Negation = A.String "negation"
  toJSON Preposition = A.String "preposition"
  toJSON RelativePronoun = A.String "relative_pronoun"
  toJSON Exceptive = A.String "exceptive"
  toJSON (ParticleFamily fam) = A.object ["type" .= ("particle_family" :: Text), "value" .= fam]
  -- Special markers
  toJSON Emphasis = A.String "emphasis"
  toJSON Vocative = A.String "vocative"
  toJSON Interrogative = A.String "interrogative"
  toJSON Answer = A.String "answer"
  toJSON Restriction = A.String "restriction"
  toJSON Circumstantial = A.String "circumstantial"
  toJSON Purpose = A.String "purpose"
  toJSON Future = A.String "future"
  toJSON Prohibition = A.String "prohibition"
  toJSON Resumption = A.String "resumption"
  toJSON Cause = A.String "cause"
  toJSON Retraction = A.String "retraction"
  toJSON Inchoative = A.String "inchoative"
  toJSON Surprise = A.String "surprise"
  toJSON Preventive = A.String "preventive"
  toJSON Amplification = A.String "amplification"
  toJSON Interpretation = A.String "interpretation"
  toJSON Certainty = A.String "certainty"
  toJSON Comitative = A.String "comitative"
  toJSON Supplemental = A.String "supplemental"
  toJSON Equalization = A.String "equalization"
  toJSON Conditional = A.String "conditional"
  toJSON Result = A.String "result"
  toJSON Location = A.String "location"
  toJSON Time = A.String "time"
  toJSON (PGN pgn) = A.object ["type" .= ("pgn" :: Text), "value" .= pgn]

-- | Morpheme segment from TSV line
data MorphSegment = MorphSegment
  { location :: MorphLocation,
    -- | Surface form (Arabic text)
    surface :: Text,
    pos :: PartOfSpeech,
    features :: [MorphFeature]
  }
  deriving (Eq, Show, Generic)

instance ToJSON MorphSegment

-- Note: FromJSON not derivable for GADTs - implement manually when needed

-- | Check if segment is a prefix
isPrefix :: MorphSegment -> Bool
isPrefix seg = Prefix `elem` features seg

-- | Check if segment is a suffix
isSuffix :: MorphSegment -> Bool
isSuffix seg = Suffix `elem` features seg

-- | Check if segment is a stem (not prefix or suffix)
isStem :: MorphSegment -> Bool
isStem seg = not (isPrefix seg || isSuffix seg)

-- | Extract root from features if present
getRoot :: MorphSegment -> Maybe Text
getRoot seg = case [r | Root r <- features seg] of
  (r : _) -> Just r
  [] -> Nothing

-- | Extract lemma from features if present
getLemma :: MorphSegment -> Maybe Text
getLemma seg = case [l | Lemma l <- features seg] of
  (l : _) -> Just l
  [] -> Nothing

-- | Aggregated Quranic word (multiple segments)
data QuranicWord = QuranicWord
  { wordLocation :: WordLocation,
    -- | Ordered segments (prefix, stem, suffix)
    segments :: [MorphSegment],
    -- | Concatenated surface form
    fullSurface :: Text,
    -- | Extracted root (if any)
    wordRoot :: Maybe Text,
    -- | Extracted lemma (if any)
    wordLemma :: Maybe Text,
    -- | POS from stem segment
    wordPOS :: PartOfSpeech
  }
  deriving (Eq, Show, Generic)

instance ToJSON QuranicWord

-- Note: FromJSON not derivable due to MorphSegment - implement manually when needed

-- | Aggregate morpheme segments into words
aggregateSegments :: [MorphSegment] -> [QuranicWord]
aggregateSegments segs =
  let grouped = groupByWord segs
   in map buildWord grouped
  where
    groupByWord :: [MorphSegment] -> [[MorphSegment]]
    groupByWord = foldr addToGroup []
      where
        addToGroup seg [] = [[seg]]
        addToGroup seg allGroups@(g : gs) =
          case g of
            (firstSeg : _) | sameWord seg firstSeg -> (seg : g) : gs
            _ -> [seg] : allGroups

        sameWord s1 s2 =
          let l1 = location s1
              l2 = location s2
           in surah l1 == surah l2
                && verse l1 == verse l2
                && word l1 == word l2

    buildWord :: [MorphSegment] -> QuranicWord
    buildWord [] = error "Cannot build word from empty segments"
    buildWord segs'@(s : _) =
      let loc = location s
          wloc = WordLocation (surah loc) (verse loc) (word loc)
          fullText = T.concat $ map surface segs'
          stemSeg = case filter isStem segs' of
            (st : _) -> st
            [] -> s -- Fallback to first segment
          root' = getRoot stemSeg
          lemma' = getLemma stemSeg
          pos' = pos stemSeg
       in QuranicWord wloc segs' fullText root' lemma' pos'

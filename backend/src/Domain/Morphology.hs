{-|
Module      : Domain.Morphology
Description : Domain types for Quranic morphological analysis
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

GADTs and type families for type-safe morphological features.
Based on Mustafa's improved Quranic Corpus (130,030 morpheme segments).
-}

module Domain.Morphology
  ( -- * Location
    MorphLocation(..)
  , mkLocation
  , compareLocation
    -- * Part of Speech
  , PartOfSpeech(..)
  , POSTag(..)
    -- * Morphological Features (GADT)
  , MorphFeature(..)
  , VerbForm(..)
  , VerbMood(..)
  , PersonGenderNumber(..)
  , NounCase(..)
  , NounNumber(..)
  , NounGender(..)
  , Definiteness(..)
    -- * Morpheme Segment
  , MorphSegment(..)
  , isPrefix
  , isSuffix
  , isStem
  , getRoot
  , getLemma
    -- * Word Aggregation
  , WordLocation(..)
  , QuranicWord(..)
  , aggregateSegments
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Location of a morpheme segment: surah:verse:word:segment
data MorphLocation = MorphLocation
  { surah   :: !Int  -- ^ Surah number (1-114)
  , verse   :: !Int  -- ^ Verse number (1-286)
  , word    :: !Int  -- ^ Word position in verse
  , segment :: !Int  -- ^ Segment position in word
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON MorphLocation
instance FromJSON MorphLocation

-- | Smart constructor with validation
mkLocation :: Int -> Int -> Int -> Int -> Maybe MorphLocation
mkLocation s v w seg
  | s < 1 || s > 114  = Nothing
  | v < 1             = Nothing
  | w < 1             = Nothing
  | seg < 1           = Nothing
  | otherwise         = Just $ MorphLocation s v w seg

-- | Compare locations for ordering
compareLocation :: MorphLocation -> MorphLocation -> Ordering
compareLocation l1 l2 =
  compare (surah l1, verse l1, word l1, segment l1)
          (surah l2, verse l2, word l2, segment l2)

-- | Word location (aggregated segments)
data WordLocation = WordLocation
  { wSurah :: !Int
  , wVerse :: !Int
  , wWord  :: !Int
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON WordLocation
instance FromJSON WordLocation

-- | Part-of-speech tag from corpus
data POSTag = N | V | P  -- Noun, Verb, Particle
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
  = Form1   -- فَعَلَ
  | Form2   -- فَعَّلَ
  | Form3   -- فَاعَلَ
  | Form4   -- أَفْعَلَ
  | Form5   -- تَفَعَّلَ
  | Form6   -- تَفَاعَلَ
  | Form7   -- انْفَعَلَ
  | Form8   -- افْتَعَلَ
  | Form9   -- افْعَلَّ
  | Form10  -- اسْتَفْعَلَ
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON VerbForm
instance FromJSON VerbForm

-- | Verbal mood
data VerbMood
  = Indicative   -- مرفوع (MOOD:IND)
  | Subjunctive  -- منصوب (MOOD:SUBJ)
  | Jussive      -- مجزوم (MOOD:JUS)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON VerbMood
instance FromJSON VerbMood

-- | Person, gender, and number combined
data PersonGenderNumber
  = FirstPersonSingular       -- 1S
  | FirstPersonPlural         -- 1P
  | SecondPersonMaleSingular  -- 2MS
  | SecondPersonFemaleSingular -- 2FS
  | SecondPersonMaleDual      -- 2MD
  | SecondPersonFemaleDual    -- 2FD
  | SecondPersonMalePlural    -- 2MP
  | SecondPersonFemalePlural  -- 2FP
  | ThirdPersonMaleSingular   -- 3MS
  | ThirdPersonFemaleSingular -- 3FS
  | ThirdPersonMaleDual       -- 3MD
  | ThirdPersonFemaleDual     -- 3FD
  | ThirdPersonMalePlural     -- 3MP
  | ThirdPersonFemalePlural   -- 3FP
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PersonGenderNumber
instance FromJSON PersonGenderNumber

-- | Noun case
data NounCase
  = Nominative   -- مرفوع (NOM)
  | Accusative   -- منصوب (ACC)
  | Genitive     -- مجرور (GEN)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NounCase
instance FromJSON NounCase

-- | Noun number
data NounNumber
  = Singular  -- (S)
  | Dual      -- (D)
  | Plural    -- (P)
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

-- | Morphological features as GADT
-- Provides compile-time guarantees about feature combinations
data MorphFeature where
  -- Universal features
  Root       :: Text -> MorphFeature
  Lemma      :: Text -> MorphFeature
  Prefix     :: MorphFeature
  Suffix     :: MorphFeature

  -- Verb features
  Perfect    :: MorphFeature
  Imperfect  :: MorphFeature
  Imperative :: MorphFeature
  VerbFormF  :: VerbForm -> MorphFeature
  Mood       :: VerbMood -> MorphFeature
  ActiveParticiple  :: MorphFeature
  PassiveParticiple :: MorphFeature

  -- Noun features
  ProperNoun :: MorphFeature
  Pronoun    :: MorphFeature
  Case       :: NounCase -> MorphFeature
  Number     :: NounNumber -> MorphFeature
  Gender     :: NounGender -> MorphFeature
  Definiteness :: Definiteness -> MorphFeature
  Adjective  :: MorphFeature

  -- Particle features
  Determiner      :: MorphFeature  -- ال
  Conjunction     :: MorphFeature  -- و، ف
  Negation        :: MorphFeature  -- لا
  Preposition     :: MorphFeature
  RelativePronoun :: MorphFeature
  Exceptive       :: MorphFeature

  -- Person/Gender/Number
  PGN :: PersonGenderNumber -> MorphFeature

deriving instance Eq MorphFeature
deriving instance Show MorphFeature

instance ToJSON MorphFeature where
  toJSON (Root t)    = A.object ["type" .= ("root" :: Text), "value" .= t]
  toJSON (Lemma t)   = A.object ["type" .= ("lemma" :: Text), "value" .= t]
  toJSON Prefix      = A.String "prefix"
  toJSON Suffix      = A.String "suffix"
  toJSON Perfect     = A.String "perfect"
  toJSON Imperfect   = A.String "imperfect"
  toJSON Imperative  = A.String "imperative"
  toJSON (VerbFormF vf) = A.object ["type" .= ("verb_form" :: Text), "value" .= vf]
  toJSON (Mood m)    = A.object ["type" .= ("mood" :: Text), "value" .= m]
  toJSON ActiveParticiple = A.String "active_participle"
  toJSON PassiveParticiple = A.String "passive_participle"
  toJSON ProperNoun  = A.String "proper_noun"
  toJSON Pronoun     = A.String "pronoun"
  toJSON (Case c)    = A.object ["type" .= ("case" :: Text), "value" .= c]
  toJSON (Number n)  = A.object ["type" .= ("number" :: Text), "value" .= n]
  toJSON (Gender g)  = A.object ["type" .= ("gender" :: Text), "value" .= g]
  toJSON (Definiteness d) = A.object ["type" .= ("definiteness" :: Text), "value" .= d]
  toJSON Adjective   = A.String "adjective"
  toJSON Determiner  = A.String "determiner"
  toJSON Conjunction = A.String "conjunction"
  toJSON Negation    = A.String "negation"
  toJSON Preposition = A.String "preposition"
  toJSON RelativePronoun = A.String "relative_pronoun"
  toJSON Exceptive   = A.String "exceptive"
  toJSON (PGN pgn)   = A.object ["type" .= ("pgn" :: Text), "value" .= pgn]

-- | Morpheme segment from TSV line
data MorphSegment = MorphSegment
  { location :: MorphLocation
  , surface  :: Text            -- ^ Surface form (Arabic text)
  , pos      :: PartOfSpeech
  , features :: [MorphFeature]
  } deriving (Eq, Show, Generic)

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
  (r:_) -> Just r
  []    -> Nothing

-- | Extract lemma from features if present
getLemma :: MorphSegment -> Maybe Text
getLemma seg = case [l | Lemma l <- features seg] of
  (l:_) -> Just l
  []    -> Nothing

-- | Aggregated Quranic word (multiple segments)
data QuranicWord = QuranicWord
  { wordLocation :: WordLocation
  , segments     :: [MorphSegment]  -- ^ Ordered segments (prefix, stem, suffix)
  , fullSurface  :: Text            -- ^ Concatenated surface form
  , wordRoot     :: Maybe Text      -- ^ Extracted root (if any)
  , wordLemma    :: Maybe Text      -- ^ Extracted lemma (if any)
  , wordPOS      :: PartOfSpeech    -- ^ POS from stem segment
  } deriving (Eq, Show, Generic)

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
        addToGroup seg allGroups@(g:gs) =
          case g of
            (firstSeg:_) | sameWord seg firstSeg -> (seg : g) : gs
            _ -> [seg] : allGroups

        sameWord s1 s2 =
          let l1 = location s1
              l2 = location s2
          in surah l1 == surah l2
             && verse l1 == verse l2
             && word l1 == word l2

    buildWord :: [MorphSegment] -> QuranicWord
    buildWord [] = error "Cannot build word from empty segments"
    buildWord segs'@(s:_) =
      let loc = location s
          wloc = WordLocation (surah loc) (verse loc) (word loc)
          fullText = T.concat $ map surface segs'
          stemSeg = case filter isStem segs' of
                      (st:_) -> st
                      []     -> s  -- Fallback to first segment
          root' = getRoot stemSeg
          lemma' = getLemma stemSeg
          pos' = pos stemSeg
      in QuranicWord wloc segs' fullText root' lemma' pos'

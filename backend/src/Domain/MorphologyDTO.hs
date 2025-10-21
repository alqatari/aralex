{-|
Module      : Domain.MorphologyDTO
Description : Data Transfer Objects for morphology (serialization layer)
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Simple ADTs for API serialization. These mirror the GADT structure from
Domain.Morphology but use regular sum types that can be auto-generated
to Purescript with purescript-bridge.

Architecture: Keep GADTs internally for type safety, use DTOs at API boundary.
-}

module Domain.MorphologyDTO
  ( -- * DTO Types
    MorphFeatureDTO(..)
  , MorphSegmentDTO(..)
  , QuranicWordDTO(..)
    -- * Conversion Functions
  , featureToDTO
  , featureFromDTO
  , segmentToDTO
  , segmentFromDTO
  , wordToDTO
  , wordFromDTO
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Domain.Morphology as M

-- | DTO for MorphFeature (serializable version of GADT)
data MorphFeatureDTO
  -- Universal features
  = RootDTO Text
  | LemmaDTO Text
  | PrefixDTO
  | SuffixDTO

  -- Verb features
  | PerfectDTO
  | ImperfectDTO
  | ImperativeDTO
  | VerbFormDTO M.VerbForm
  | MoodDTO M.VerbMood
  | ActiveParticipleDTO
  | PassiveParticipleDTO

  -- Noun features
  | ProperNounDTO
  | PronounDTO
  | CaseDTO M.NounCase
  | NumberDTO M.NounNumber
  | GenderDTO M.NounGender
  | DefinitenessDTO M.Definiteness
  | AdjectiveDTO

  -- Particle features
  | DeterminerDTO
  | ConjunctionDTO
  | NegationDTO
  | PrepositionDTO
  | RelativePronounDTO
  | ExceptiveDTO

  -- Person/Gender/Number
  | PGNDTO M.PersonGenderNumber
  deriving (Eq, Show, Generic)

instance ToJSON MorphFeatureDTO
instance FromJSON MorphFeatureDTO

-- | DTO for MorphSegment
data MorphSegmentDTO = MorphSegmentDTO
  { dtoLocation :: M.MorphLocation
  , dtoSurface  :: Text
  , dtoPos      :: M.PartOfSpeech
  , dtoFeatures :: [MorphFeatureDTO]
  } deriving (Eq, Show, Generic)

instance ToJSON MorphSegmentDTO
instance FromJSON MorphSegmentDTO

-- | DTO for QuranicWord
data QuranicWordDTO = QuranicWordDTO
  { dtoWordLocation :: M.WordLocation
  , dtoSegments     :: [MorphSegmentDTO]
  , dtoFullSurface  :: Text
  , dtoWordRoot     :: Maybe Text
  , dtoWordLemma    :: Maybe Text
  , dtoWordPOS      :: M.PartOfSpeech
  } deriving (Eq, Show, Generic)

instance ToJSON QuranicWordDTO
instance FromJSON QuranicWordDTO

-- | Convert GADT MorphFeature to DTO
featureToDTO :: M.MorphFeature -> MorphFeatureDTO
featureToDTO = \case
  M.Root t              -> RootDTO t
  M.Lemma t             -> LemmaDTO t
  M.Prefix              -> PrefixDTO
  M.Suffix              -> SuffixDTO
  M.Perfect             -> PerfectDTO
  M.Imperfect           -> ImperfectDTO
  M.Imperative          -> ImperativeDTO
  M.VerbFormF vf        -> VerbFormDTO vf
  M.Mood m              -> MoodDTO m
  M.ActiveParticiple    -> ActiveParticipleDTO
  M.PassiveParticiple   -> PassiveParticipleDTO
  M.ProperNoun          -> ProperNounDTO
  M.Pronoun             -> PronounDTO
  M.Case c              -> CaseDTO c
  M.Number n            -> NumberDTO n
  M.Gender g            -> GenderDTO g
  M.Definiteness d      -> DefinitenessDTO d
  M.Adjective           -> AdjectiveDTO
  M.Determiner          -> DeterminerDTO
  M.Conjunction         -> ConjunctionDTO
  M.Negation            -> NegationDTO
  M.Preposition         -> PrepositionDTO
  M.RelativePronoun     -> RelativePronounDTO
  M.Exceptive           -> ExceptiveDTO
  M.PGN pgn             -> PGNDTO pgn

-- | Convert DTO back to GADT MorphFeature
featureFromDTO :: MorphFeatureDTO -> M.MorphFeature
featureFromDTO = \case
  RootDTO t              -> M.Root t
  LemmaDTO t             -> M.Lemma t
  PrefixDTO              -> M.Prefix
  SuffixDTO              -> M.Suffix
  PerfectDTO             -> M.Perfect
  ImperfectDTO           -> M.Imperfect
  ImperativeDTO          -> M.Imperative
  VerbFormDTO vf         -> M.VerbFormF vf
  MoodDTO m              -> M.Mood m
  ActiveParticipleDTO    -> M.ActiveParticiple
  PassiveParticipleDTO   -> M.PassiveParticiple
  ProperNounDTO          -> M.ProperNoun
  PronounDTO             -> M.Pronoun
  CaseDTO c              -> M.Case c
  NumberDTO n            -> M.Number n
  GenderDTO g            -> M.Gender g
  DefinitenessDTO d      -> M.Definiteness d
  AdjectiveDTO           -> M.Adjective
  DeterminerDTO          -> M.Determiner
  ConjunctionDTO         -> M.Conjunction
  NegationDTO            -> M.Negation
  PrepositionDTO         -> M.Preposition
  RelativePronounDTO     -> M.RelativePronoun
  ExceptiveDTO           -> M.Exceptive
  PGNDTO pgn             -> M.PGN pgn

-- | Convert MorphSegment to DTO
segmentToDTO :: M.MorphSegment -> MorphSegmentDTO
segmentToDTO seg = MorphSegmentDTO
  { dtoLocation = M.location seg
  , dtoSurface  = M.surface seg
  , dtoPos      = M.pos seg
  , dtoFeatures = map featureToDTO (M.features seg)
  }

-- | Convert DTO to MorphSegment
segmentFromDTO :: MorphSegmentDTO -> M.MorphSegment
segmentFromDTO dto = M.MorphSegment
  { M.location = dtoLocation dto
  , M.surface  = dtoSurface dto
  , M.pos      = dtoPos dto
  , M.features = map featureFromDTO (dtoFeatures dto)
  }

-- | Convert QuranicWord to DTO
wordToDTO :: M.QuranicWord -> QuranicWordDTO
wordToDTO word = QuranicWordDTO
  { dtoWordLocation = M.wordLocation word
  , dtoSegments     = map segmentToDTO (M.segments word)
  , dtoFullSurface  = M.fullSurface word
  , dtoWordRoot     = M.wordRoot word
  , dtoWordLemma    = M.wordLemma word
  , dtoWordPOS      = M.wordPOS word
  }

-- | Convert DTO to QuranicWord
wordFromDTO :: QuranicWordDTO -> M.QuranicWord
wordFromDTO dto = M.QuranicWord
  { M.wordLocation = dtoWordLocation dto
  , M.segments     = map segmentFromDTO (dtoSegments dto)
  , M.fullSurface  = dtoFullSurface dto
  , M.wordRoot     = dtoWordRoot dto
  , M.wordLemma    = dtoWordLemma dto
  , M.wordPOS      = dtoWordPOS dto
  }

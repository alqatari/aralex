-- | Grammar terminology mapping (Arabic only)
-- | Maps morphological features to Arabic grammatical terms
module Util.Grammar where

import Prelude
import Data.Maybe (Maybe(..))
import Domain.MorphologyDTO
import Domain.Morphology

-- | Get Arabic label for part of speech
posToArabic :: PartOfSpeech -> String
posToArabic = case _ of
  Noun -> "اسم"
  Verb -> "فعل"
  Particle -> "حرف"

-- | Get Arabic label for noun case
caseToArabic :: NounCase -> String
caseToArabic = case _ of
  Nominative -> "مرفوع"
  Accusative -> "منصوب"
  Genitive -> "مجرور"

-- | Get Arabic label for noun number
numberToArabic :: NounNumber -> String
numberToArabic = case _ of
  Singular -> "مفرد"
  Dual -> "مثنى"
  Plural -> "جمع"

-- | Get Arabic label for noun gender
genderToArabic :: NounGender -> String
genderToArabic = case _ of
  Masculine -> "مذكر"
  Feminine -> "مؤنث"

-- | Get Arabic label for definiteness
definitenessToArabic :: Definiteness -> String
definitenessToArabic = case _ of
  Definite -> "معرفة"
  Indefinite -> "نكرة"

-- | Get Arabic label for verb form
verbFormToArabic :: VerbForm -> String
verbFormToArabic = case _ of
  Form1 -> "الأول (فَعَلَ)"
  Form2 -> "الثاني (فَعَّلَ)"
  Form3 -> "الثالث (فَاعَلَ)"
  Form4 -> "الرابع (أَفْعَلَ)"
  Form5 -> "الخامس (تَفَعَّلَ)"
  Form6 -> "السادس (تَفَاعَلَ)"
  Form7 -> "السابع (انْفَعَلَ)"
  Form8 -> "الثامن (افْتَعَلَ)"
  Form9 -> "التاسع (افْعَلَّ)"
  Form10 -> "العاشر (اسْتَفْعَلَ)"

-- | Get Arabic label for verb mood
moodToArabic :: VerbMood -> String
moodToArabic = case _ of
  Indicative -> "مرفوع"
  Subjunctive -> "منصوب"
  Jussive -> "مجزوم"

-- | Get Arabic label for voice
voiceToArabic :: Voice -> String
voiceToArabic = case _ of
  Active -> "مبني للمعلوم"
  Passive -> "مبني للمجهول"

-- | Get Arabic label for person-gender-number
pgnToArabic :: PersonGenderNumber -> String
pgnToArabic = case _ of
  FirstPersonSingular -> "المتكلم المفرد"
  FirstPersonPlural -> "المتكلم الجمع"
  SecondPersonMaleSingular -> "المخاطب المذكر المفرد"
  SecondPersonFemaleSingular -> "المخاطب المؤنث المفرد"
  SecondPersonMaleDual -> "المخاطب المذكر المثنى"
  SecondPersonFemaleDual -> "المخاطب المؤنث المثنى"
  SecondPersonMalePlural -> "المخاطب المذكر الجمع"
  SecondPersonFemalePlural -> "المخاطب المؤنث الجمع"
  ThirdPersonMaleSingular -> "الغائب المذكر المفرد"
  ThirdPersonFemaleSingular -> "الغائب المؤنث المفرد"
  ThirdPersonMaleDual -> "الغائب المذكر المثنى"
  ThirdPersonFemaleDual -> "الغائب المؤنث المثنى"
  ThirdPersonMalePlural -> "الغائب المذكر الجمع"
  ThirdPersonFemalePlural -> "الغائب المؤنث الجمع"

-- | Get Arabic label for particle family
particleFamilyToArabic :: ParticleFamily -> String
particleFamilyToArabic = case _ of
  InnFamily -> "إن وأخواتها"
  KanaFamily -> "كان وأخواتها"
  LawFamily -> "لو وأخواتها"
  ZannFamily -> "ظن وأخواتها"
  RajaaFamily -> "رجا وأخواتها"
  OtherFamily name -> name

-- | Get Arabic description for verb tense
verbTenseToArabic :: MorphFeatureDTO -> Maybe String
verbTenseToArabic = case _ of
  PerfectDTO -> Just "ماضي"
  ImperfectDTO -> Just "مضارع"
  ImperativeDTO -> Just "أمر"
  _ -> Nothing

-- | Get short Arabic label for feature (for badges)
featureToShortArabic :: MorphFeatureDTO -> Maybe String
featureToShortArabic = case _ of
  -- Noun features
  CaseDTO c -> Just $ caseToArabic c
  NumberDTO n -> Just $ numberToArabic n
  GenderDTO g -> Just $ genderToArabic g
  DefinitenessDTO d -> Just $ definitenessToArabic d

  -- Verb features
  PerfectDTO -> Just "ماضي"
  ImperfectDTO -> Just "مضارع"
  ImperativeDTO -> Just "أمر"
  VerbFormDTO vf -> Just $ "الوزن " <> verbFormToArabic vf
  MoodDTO m -> Just $ moodToArabic m
  VoiceDTO v -> Just $ voiceToArabic v

  -- POS-related
  ProperNounDTO -> Just "علم"
  PronounDTO -> Just "ضمير"
  AdjectiveDTO -> Just "صفة"
  ActiveParticipleDTO -> Just "اسم فاعل"
  PassiveParticipleDTO -> Just "اسم مفعول"

  -- Particles
  DeterminerDTO -> Just "أداة تعريف"
  ConjunctionDTO -> Just "حرف عطف"
  NegationDTO -> Just "حرف نفي"
  PrepositionDTO -> Just "حرف جر"

  -- Special markers
  EmphasisDTO -> Just "لام التوكيد"
  VocativeDTO -> Just "حرف نداء"
  InterrogativeDTO -> Just "حرف استفهام"
  FutureDTO -> Just "حرف استقبال"
  ProhibitionDTO -> Just "لا الناهية"

  -- Particle families
  ParticleFamilyDTO fam -> Just $ particleFamilyToArabic fam

  _ -> Nothing

-- | Get color for part of speech
posColor :: PartOfSpeech -> String
posColor = case _ of
  Noun -> "#2C5282"      -- Deep blue
  Verb -> "#2F855A"      -- Green
  Particle -> "#6B46C1"  -- Purple

-- | Get background color for case
caseBackgroundColor :: NounCase -> String
caseBackgroundColor = case _ of
  Nominative -> "#EBF8FF"  -- Light blue
  Accusative -> "#F0FFF4"  -- Light green
  Genitive -> "#FAF5FF"    -- Light purple

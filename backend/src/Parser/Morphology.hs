{-|
Module      : Parser.Morphology
Description : TSV parser for Quranic morphology data
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Parses TSV morphology file (130,030 morpheme segments).
Format: location → surface → POS → features
-}

module Parser.Morphology
  ( -- * Parsing
    parseMorphLine
  , parseMorphFile
  , parseMorphSegments
    -- * Loading
  , loadMorphologyData
    -- * Feature Parsing
  , parseFeatures
  , parseLocation
  , parsePOSTag
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text as A
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Domain.Morphology

-- | Parse location string: "surah:verse:word:segment"
parseLocation :: Parser MorphLocation
parseLocation = do
  s <- decimal <* char ':'
  v <- decimal <* char ':'
  w <- decimal <* char ':'
  seg <- decimal
  case mkLocation s v w seg of
    Just loc -> pure loc
    Nothing  -> fail $ "Invalid location: " <> show (s, v, w, seg)

-- | Parse POS tag (N, V, P)
parsePOSTag :: Parser PartOfSpeech
parsePOSTag =
      (char 'N' *> pure Noun)
  <|> (char 'V' *> pure Verb)
  <|> (char 'P' *> pure Particle)

-- | Parse pipe-delimited features
parseFeatures :: Parser [MorphFeature]
parseFeatures = feature `sepBy` char '|'
  where
    feature :: Parser MorphFeature
    feature =
      -- Root and Lemma
          (string "ROOT:" *> (Root <$> A.takeWhile1 (/= '|')))
      <|> (string "LEM:" *> (Lemma <$> A.takeWhile1 (/= '|')))
      -- Prefix/Suffix
      <|> (string "PREF" *> pure Prefix)
      <|> (string "SUFF" *> pure Suffix)
      -- Verb aspect
      <|> (string "PERF" *> pure Perfect)
      <|> (string "IMPF" *> pure Imperfect)
      <|> (string "IMPV" *> pure Imperative)
      -- Verb form
      <|> (string "VF:" *> (VerbFormF <$> parseVerbForm))
      -- Mood
      <|> (string "MOOD:IND" *> pure (Mood Indicative))
      <|> (string "MOOD:SUBJ" *> pure (Mood Subjunctive))
      <|> (string "MOOD:JUS" *> pure (Mood Jussive))
      -- Participles
      <|> (string "ACT_PCPL" *> pure ActiveParticiple)
      <|> (string "PASS_PCPL" *> pure PassiveParticiple)
      -- Noun type
      <|> (string "PN" *> pure ProperNoun)
      <|> (string "PRON" *> pure Pronoun)
      <|> (string "ADJ" *> pure Adjective)
      -- Case
      <|> (string "NOM" *> pure (Case Nominative))
      <|> (string "ACC" *> pure (Case Accusative))
      <|> (string "GEN" *> pure (Case Genitive))
      -- Number
      <|> (string "S" *> pure (Number Singular))
      <|> (string "D" *> pure (Number Dual))
      <|> (string "P" *> pure (Number Plural))
      -- Gender
      <|> (string "M" *> pure (Gender Masculine))
      <|> (string "F" *> pure (Gender Feminine))
      -- Definiteness
      <|> (string "INDEF" *> pure (Definiteness Indefinite))
      <|> (string "DEF" *> pure (Definiteness Domain.Morphology.Definite))
      -- Particle types
      <|> (string "DET" *> pure Determiner)
      <|> (string "CONJ" *> pure Conjunction)
      <|> (string "NEG" *> pure Negation)
      <|> (string "P" *> pure Preposition)
      <|> (string "REL" *> pure RelativePronoun)
      <|> (string "EXP" *> pure Exceptive)
      -- Person/Gender/Number
      <|> parsePGN
      -- Fallback for unknown features (ignore)
      <|> (A.takeWhile1 (/= '|') *> fail "Unknown feature")

    parseVerbForm :: Parser VerbForm
    parseVerbForm = do
      n <- decimal
      case n :: Int of
        1  -> pure Form1
        2  -> pure Form2
        3  -> pure Form3
        4  -> pure Form4
        5  -> pure Form5
        6  -> pure Form6
        7  -> pure Form7
        8  -> pure Form8
        9  -> pure Form9
        10 -> pure Form10
        _  -> fail $ "Invalid verb form: " <> show n

    parsePGN :: Parser MorphFeature
    parsePGN =
          (string "1S" *> pure (PGN FirstPersonSingular))
      <|> (string "1P" *> pure (PGN FirstPersonPlural))
      <|> (string "2MS" *> pure (PGN SecondPersonMaleSingular))
      <|> (string "2FS" *> pure (PGN SecondPersonFemaleSingular))
      <|> (string "2MD" *> pure (PGN SecondPersonMaleDual))
      <|> (string "2FD" *> pure (PGN SecondPersonFemaleDual))
      <|> (string "2MP" *> pure (PGN SecondPersonMalePlural))
      <|> (string "2FP" *> pure (PGN SecondPersonFemalePlural))
      <|> (string "3MS" *> pure (PGN ThirdPersonMaleSingular))
      <|> (string "3FS" *> pure (PGN ThirdPersonFemaleSingular))
      <|> (string "3MD" *> pure (PGN ThirdPersonMaleDual))
      <|> (string "3FD" *> pure (PGN ThirdPersonFemaleDual))
      <|> (string "3MP" *> pure (PGN ThirdPersonMalePlural))
      <|> (string "3FP" *> pure (PGN ThirdPersonFemalePlural))

-- | Parse single TSV line into MorphSegment
parseMorphLine :: Parser MorphSegment
parseMorphLine = do
  loc <- parseLocation
  _ <- char '\t'
  surf <- A.takeWhile (/= '\t')  -- Allow empty surface forms (implicit morphemes)
  _ <- char '\t'
  pos <- parsePOSTag
  _ <- char '\t'
  -- Get features text and strip trailing pipes
  featText <- A.takeText
  let featText' = T.dropWhileEnd (== '|') featText
  case parseOnly parseFeatures featText' of
    Left err -> fail $ "Failed to parse features: " <> err
    Right feats -> pure $ MorphSegment loc surf pos feats

-- | Parse entire morphology file
parseMorphSegments :: Text -> Either String [MorphSegment]
parseMorphSegments content =
  let linesWithNum = zip [1..] (T.lines content)
  in mapM parseLineWithNum linesWithNum
  where
    parseLineWithNum :: (Int, Text) -> Either String MorphSegment
    parseLineWithNum (lineNum, line) =
      case parseOnly parseMorphLine line of
        Left err -> Left $ "Line " <> show lineNum <> ": " <> err
        Right seg -> Right seg

-- | Load and parse morphology file
parseMorphFile :: FilePath -> IO (Either String [MorphSegment])
parseMorphFile path = do
  content <- TIO.readFile path
  pure $ parseMorphSegments content

-- | Load morphology data from data directory
loadMorphologyData :: FilePath -> IO (Either String [MorphSegment])
loadMorphologyData dataDir = do
  let morphPath = dataDir ++ "/quran_corpus/quran-morphology.txt"
  parseMorphFile morphPath

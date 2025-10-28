-- |
-- Module      : Bridge
-- Description : Purescript-bridge type generation configuration
-- Copyright   : (c) Ali Al-Qatari, 2025
-- License     : MIT
--
-- Defines which Haskell types to generate Purescript equivalents for.
-- This enables type-safe communication between Haskell backend and Purescript frontend.
module Bridge
  ( generateTypes,
    myTypes,
  )
where

import Data.Proxy (Proxy (..))
-- Import domain types
import Data.QuranMetadata
import Domain.Dictionary
import Domain.EtymologyGraph
import Domain.Irab
import Domain.Morphology
import Domain.MorphologyDTO
import Domain.Phonosemantics
import Domain.Verse
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

-- | All types to bridge to Purescript
myTypes :: [SumType 'Haskell]
myTypes =
  -- Dictionary types
  [ mkSumType (Proxy :: Proxy DictionaryId),
    mkSumType (Proxy :: Proxy DictionarySource),
    mkSumType (Proxy :: Proxy RootText),
    mkSumType (Proxy :: Proxy DictEntry),
    -- Morphology types (base types)
    mkSumType (Proxy :: Proxy MorphLocation),
    mkSumType (Proxy :: Proxy WordLocation),
    mkSumType (Proxy :: Proxy POSTag),
    mkSumType (Proxy :: Proxy PartOfSpeech),
    mkSumType (Proxy :: Proxy VerbForm),
    mkSumType (Proxy :: Proxy VerbMood),
    mkSumType (Proxy :: Proxy PersonGenderNumber),
    mkSumType (Proxy :: Proxy NounCase),
    mkSumType (Proxy :: Proxy NounNumber),
    mkSumType (Proxy :: Proxy NounGender),
    mkSumType (Proxy :: Proxy Definiteness),
    mkSumType (Proxy :: Proxy Voice),
    mkSumType (Proxy :: Proxy ParticleFamily),
    -- Morphology DTOs (serialization layer - works with purescript-bridge)
    mkSumType (Proxy :: Proxy MorphFeatureDTO),
    mkSumType (Proxy :: Proxy MorphSegmentDTO),
    mkSumType (Proxy :: Proxy QuranicWordDTO),
    -- Verse and Quran metadata types
    mkSumType (Proxy :: Proxy RevelationType),
    mkSumType (Proxy :: Proxy SurahNumber),
    mkSumType (Proxy :: Proxy VerseCount),
    mkSumType (Proxy :: Proxy RevelationOrder),
    mkSumType (Proxy :: Proxy RukuCount),
    mkSumType (Proxy :: Proxy SurahName),
    mkSumType (Proxy :: Proxy SurahInfo),
    mkSumType (Proxy :: Proxy VerseRef),
    mkSumType (Proxy :: Proxy VerseText),
    mkSumType (Proxy :: Proxy VerseWithRoot),
    -- Phonosemantics types
    mkSumType (Proxy :: Proxy ArabicLetter),
    mkSumType (Proxy :: Proxy LetterCategory),
    mkSumType (Proxy :: Proxy ArticulationPoint),
    mkSumType (Proxy :: Proxy LetterAttribute),
    mkSumType (Proxy :: Proxy SemanticDomain),
    mkSumType (Proxy :: Proxy LetterMeaning),
    mkSumType (Proxy :: Proxy LetterBreakdown),
    mkSumType (Proxy :: Proxy PhonosemanticPattern),
    mkSumType (Proxy :: Proxy DictionaryEvidence),
    mkSumType (Proxy :: Proxy QuranicEvidence),
    mkSumType (Proxy :: Proxy WordAnalysis),
    -- Etymology Graph types
    mkSumType (Proxy :: Proxy NodeType),
    mkSumType (Proxy :: Proxy EdgeType),
    mkSumType (Proxy :: Proxy NodePosition),
    mkSumType (Proxy :: Proxy GraphNode),
    mkSumType (Proxy :: Proxy GraphEdge),
    mkSumType (Proxy :: Proxy EtymologyGraph),
    -- I'rab types
    mkSumType (Proxy :: Proxy IrabSource),
    mkSumType (Proxy :: Proxy IrabDetail)
  ]

-- | Bridge configuration
-- Handles GHC 9.12+ where [] is represented as GHC.Types.List
bridge :: BridgePart
bridge =
  defaultBridge
    <|> (typeName ^== "Char" >> return (TypeInfo "" "Prim" "Char" []))
    <|> ( do
            typeName ^== "List"
            typeModule ^== "GHC.Types"
            psArray
        )

-- | Generate Purescript types to specified directory
generateTypes :: FilePath -> IO ()
generateTypes outputDir = do
  writePSTypes outputDir (buildBridge bridge) myTypes
  putStrLn $ "Generated Purescript types in: " <> outputDir

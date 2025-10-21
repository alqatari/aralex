{-|
Module      : Domain.EtymologyGraph
Description : Etymology graph domain types for phonosemantic visualization
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Types for representing etymology graphs showing relationships between
roots, letters, and semantic concepts based on Hassan Abbas's phonosemantic theory.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.EtymologyGraph
  ( EtymologyGraph(..)
  , GraphNode(..)
  , GraphEdge(..)
  , NodeType(..)
  , EdgeType(..)
  , NodePosition(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Domain.Dictionary (RootText)
import Domain.Phonosemantics (ArabicLetter)

-- | Type of node in the etymology graph
data NodeType
  = TargetRoot      -- ^ The root word being analyzed
  | LetterNode      -- ^ Individual letter from the root
  | RelatedRoot     -- ^ Related roots sharing letters
  | SemanticConcept -- ^ Semantic concept derived from letter meanings
  deriving (Show, Eq, Generic)

instance ToJSON NodeType
instance FromJSON NodeType

-- | Type of edge connection
data EdgeType
  = Contains        -- ^ Root contains letter
  | SharesLetter    -- ^ Two roots share a letter
  | SemanticLink    -- ^ Semantic relationship
  | EtymonPattern   -- ^ Binary etymon pattern (Bohas theory)
  deriving (Show, Eq, Generic)

instance ToJSON EdgeType
instance FromJSON EdgeType

-- | Position in 2D space for graph layout
data NodePosition = NodePosition
  { posX :: Double
  , posY :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON NodePosition
instance FromJSON NodePosition

-- | Node in the etymology graph
data GraphNode = GraphNode
  { nodeId :: Text               -- ^ Unique identifier
  , nodeType :: NodeType         -- ^ Type of node
  , arabicText :: Text           -- ^ Arabic text to display
  , meaning :: Maybe Text        -- ^ Meaning (for letters/concepts)
  , occurrences :: Maybe Int     -- ^ Quranic frequency (for roots)
  , position :: NodePosition     -- ^ Initial position for layout
  } deriving (Show, Eq, Generic)

instance ToJSON GraphNode
instance FromJSON GraphNode

-- | Edge connecting two nodes
data GraphEdge = GraphEdge
  { edgeId :: Text          -- ^ Unique identifier
  , fromNodeId :: Text      -- ^ Source node ID
  , toNodeId :: Text        -- ^ Target node ID
  , edgeType :: EdgeType    -- ^ Type of relationship
  , strength :: Double      -- ^ Strength/weight (0.0 to 1.0)
  , label :: Maybe Text     -- ^ Optional label
  } deriving (Show, Eq, Generic)

instance ToJSON GraphEdge
instance FromJSON GraphEdge

-- | Complete etymology graph
data EtymologyGraph = EtymologyGraph
  { graphNodes :: [GraphNode]
  , graphEdges :: [GraphEdge]
  , centerRoot :: RootText     -- ^ The central root being analyzed
  , metadata :: Text           -- ^ Additional metadata (JSON)
  } deriving (Show, Eq, Generic)

instance ToJSON EtymologyGraph
instance FromJSON EtymologyGraph

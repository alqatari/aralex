{-|
Module      : API.EtymologyGraph
Description : Etymology graph API handlers
Copyright   : (c) Ali Al-Qatari, 2025
License     : MIT

Generates etymology graphs showing phonosemantic relationships.
-}

{-# LANGUAGE OverloadedStrings #-}

module API.EtymologyGraph
  ( buildEtymologyGraph
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Control.Monad (forM)

import Database.Schema (DatabaseConnections(..))
import Domain.Dictionary (RootText(..))
import Domain.Phonosemantics (ArabicLetter(..), extractLetters)
import Domain.EtymologyGraph

-- | Build etymology graph for a root word
buildEtymologyGraph :: DatabaseConnections -> RootText -> IO EtymologyGraph
buildEtymologyGraph (DatabaseConnections dictConn quranConn) root@(RootText rootText) = do
  -- 1. Extract letters from root
  let letters = extractLetters rootText

  -- 2. Create target root node
  rootOccurrences <- getRootOccurrences quranConn rootText
  let targetNode = GraphNode
        { nodeId = "root-" <> rootText
        , nodeType = TargetRoot
        , arabicText = rootText
        , meaning = Nothing
        , occurrences = Just rootOccurrences
        , position = NodePosition 0 0  -- Center
        }

  -- 3. Create letter nodes
  letterNodes <- forM (zip [1..] letters) $ \(idx, letter@(ArabicLetter letterText)) -> do
    letterMeaning <- getLetterMeaning quranConn letterText
    let angle = (fromIntegral idx / fromIntegral (length letters)) * 2 * pi
        radius = 150.0
    pure $ GraphNode
      { nodeId = "letter-" <> letterText <> "-" <> T.pack (show idx)
      , nodeType = LetterNode
      , arabicText = letterText
      , meaning = letterMeaning
      , occurrences = Nothing
      , position = NodePosition (radius * cos angle) (radius * sin angle)
      }

  -- 4. Create edges from root to letters
  let letterEdges = map (\node -> GraphEdge
        { edgeId = "contains-" <> nodeId node
        , fromNodeId = nodeId targetNode
        , toNodeId = nodeId node
        , edgeType = Contains
        , strength = 1.0
        , label = Nothing
        }) letterNodes

  -- 5. Find related roots (share at least 2 letters)
  relatedRoots <- findRelatedRoots dictConn letters rootText

  relatedNodes <- forM (zip [1..] relatedRoots) $ \(idx, (relRoot, sharedCount)) -> do
    relOccurrences <- getRootOccurrences quranConn relRoot
    let angle = (fromIntegral idx / fromIntegral (max 1 $ length relatedRoots)) * 2 * pi
        radius = 250.0
    pure $ GraphNode
      { nodeId = "related-" <> relRoot
      , nodeType = RelatedRoot
      , arabicText = relRoot
      , meaning = Nothing
      , occurrences = Just relOccurrences
      , position = NodePosition (radius * cos angle) (radius * sin angle)
      }

  -- 6. Create edges to related roots
  let relatedEdges = map (\node -> GraphEdge
        { edgeId = "shares-" <> nodeId node
        , fromNodeId = nodeId targetNode
        , toNodeId = nodeId node
        , edgeType = SharesLetter
        , strength = 0.5
        , label = Nothing
        }) relatedNodes

  -- 7. Create semantic concept nodes (one per letter)
  let semanticNodes = zipWith (\idx (GraphNode {meaning = Just m, nodeId = lid}) ->
        let angle = (fromIntegral idx / fromIntegral (length letterNodes)) * 2 * pi
            radius = 400.0
        in GraphNode
          { nodeId = "semantic-" <> T.pack (show idx)
          , nodeType = SemanticConcept
          , arabicText = m  -- Use the letter meaning as the concept
          , meaning = Just m
          , occurrences = Nothing
          , position = NodePosition (radius * cos angle) (radius * sin angle)
          }
        ) [1..] (filter (\n -> case meaning n of Just _ -> True; _ -> False) letterNodes)

  -- 8. Create semantic link edges (letter → semantic concept)
  let semanticEdges = zipWith (\letterNode semanticNode -> GraphEdge
        { edgeId = "semantic-link-" <> nodeId letterNode
        , fromNodeId = nodeId letterNode
        , toNodeId = nodeId semanticNode
        , edgeType = SemanticLink
        , strength = 0.3
        , label = Nothing
        }) letterNodes semanticNodes

  -- 9. Create binary etymon edges (letter → letter pairs)
  let etymonEdges = if length letterNodes >= 2
        then zipWith (\i j -> GraphEdge
          { edgeId = "etymon-" <> T.pack (show i) <> "-" <> T.pack (show j)
          , fromNodeId = nodeId (letterNodes !! i)
          , toNodeId = nodeId (letterNodes !! ((i + 1) `mod` length letterNodes))
          , edgeType = EtymonPattern
          , strength = 0.7
          , label = Just $ arabicText (letterNodes !! i) <> "•" <> arabicText (letterNodes !! ((i + 1) `mod` length letterNodes))
          }) [0..length letterNodes - 1] [1..length letterNodes]
        else []

  pure $ EtymologyGraph
    { graphNodes = targetNode : (letterNodes ++ relatedNodes ++ semanticNodes)
    , graphEdges = letterEdges ++ relatedEdges ++ semanticEdges ++ etymonEdges
    , centerRoot = root
    , metadata = "{}"
    }

-- | Get number of Quranic occurrences for a root
getRootOccurrences :: Connection -> Text -> IO Int
getRootOccurrences conn rootText = do
  results <- query conn
    "SELECT COUNT(*) FROM quranic_words WHERE root = ?"
    (Only rootText) :: IO [Only Int]
  case results of
    (Only count:_) -> pure count
    _ -> pure 0

-- | Get letter meaning
getLetterMeaning :: Connection -> Text -> IO (Maybe Text)
getLetterMeaning conn letterText = do
  results <- query conn
    "SELECT primary_meaning_ar FROM letter_meanings WHERE letter = ?"
    (Only letterText) :: IO [Only Text]
  case results of
    (Only meaning:_) -> pure (Just meaning)
    _ -> pure Nothing

-- | Find roots that share letters with the target root
findRelatedRoots :: Connection -> [ArabicLetter] -> Text -> IO [(Text, Int)]
findRelatedRoots conn letters excludeRoot = do
  -- For simplicity, find roots containing at least one of the same letters
  -- TODO: Implement more sophisticated similarity matching
  let letterTexts = [t | ArabicLetter t <- letters]
  if null letterTexts
    then pure []
    else do
      -- Find roots containing any of these letters
      results <- query conn
        "SELECT DISTINCT root_normalized FROM dictionary_entries \
        \WHERE root_normalized != ? \
        \AND (root_normalized LIKE ? OR root_normalized LIKE ? OR root_normalized LIKE ?) \
        \LIMIT 5"
        (excludeRoot
        , "%" <> head letterTexts <> "%"
        , if length letterTexts > 1 then "%" <> (letterTexts !! 1) <> "%" else "%"
        , if length letterTexts > 2 then "%" <> (letterTexts !! 2) <> "%" else "%"
        ) :: IO [Only Text]

      pure $ map (\(Only root) -> (root, 2)) results  -- Placeholder shared count

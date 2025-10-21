module Component.EtymologyGraphSimple where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Array (length)
import Data.Array as Array
import Data.Number (log) as Number
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Color (Color(..))
import Halogen.Svg.Attributes.FontSize (FontSize(..))
import Halogen.Svg.Attributes.FontWeight (FontWeight(..))
import Halogen.Svg.Attributes.CSSLength (CSSLength(..))
import Halogen.Svg.Elements as SE

import Domain.EtymologyGraph (EtymologyGraph(..), GraphNode(..), GraphEdge(..), NodeType(..), EdgeType(..), NodePosition(..))
import Domain.Dictionary (RootText(..))

type Input = String  -- Root text from parent

type State =
  { graphData :: Maybe EtymologyGraph
  , loading :: Boolean
  , error :: Maybe String
  , currentRoot :: String  -- Track what root we loaded
  , zoom :: Number
  , selectedNode :: Maybe String
  , hoveredNode :: Maybe String
  , showLabels :: Boolean
  , filterNodeType :: Maybe NodeType
  }

type ViewBox =
  { minX :: Number
  , minY :: Number
  , width :: Number
  , height :: Number
  }

data Action
  = Initialize
  | Receive Input  -- Receive new root from parent
  | ZoomIn
  | ZoomOut
  | ResetView
  | SelectNode String
  | HoverNode String
  | UnhoverNode
  | ToggleLabels
  | SetFilter (Maybe NodeType)

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState root =
  { graphData: Nothing
  , loading: false
  , error: Nothing
  , currentRoot: root
  , zoom: 1.0
  , selectedNode: Nothing
  , hoveredNode: Nothing
  , showLabels: true
  , filterNodeType: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "etymology-graph-container") ]
    [ case state.error of
        Just err ->
          HH.div
            [ HP.class_ (HH.ClassName "graph-error")
            , HP.style "padding: 20px; text-align: center; color: #dc2626; background: #fee2e2; border-radius: 8px; margin: 10px;"
            ]
            [ HH.text ("خطأ: " <> err) ]
        Nothing -> HH.text ""
    , if state.loading
        then HH.div
            [ HP.style "padding: 40px; text-align: center; color: #6366f1;" ]
            [ HH.text "جاري تحميل الشبكة..." ]
        else case state.graphData of
          Nothing ->
            HH.div
              [ HP.style "padding: 40px; text-align: center; color: #9ca3af; direction: rtl;" ]
              [ HH.text "ابحث عن جذر لرؤية شبكة العلاقات الاشتقاقية" ]
          Just graph ->
            renderGraphData state graph
    ]

renderGraphData :: forall m. State -> EtymologyGraph -> H.ComponentHTML Action () m
renderGraphData state (EtymologyGraph graph) =
  HH.div
    [ HP.class_ (HH.ClassName "graph-data-display") ]
    [ renderInfoPanel state graph
    , HH.div
        [ HP.class_ (HH.ClassName "graph-controls-bar")
        , HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 12px; direction: rtl;"
        ]
        [ HH.span
            [ HP.class_ (HH.ClassName "graph-info")
            , HP.style "margin-left: 12px;"
            ]
            [ HH.text $ "عُقد: " <> show (length graph.graphNodes) <> " | حواف: " <> show (length graph.graphEdges) ]
        , HH.button
            [ HE.onClick \_ -> ZoomIn
            , HP.class_ (HH.ClassName "zoom-btn")
            , HP.style "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; background: white;"
            ]
            [ HH.text "🔍+" ]
        , HH.button
            [ HE.onClick \_ -> ZoomOut
            , HP.class_ (HH.ClassName "zoom-btn")
            , HP.style "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; background: white;"
            ]
            [ HH.text "🔍-" ]
        , HH.button
            [ HE.onClick \_ -> ToggleLabels
            , HP.class_ (HH.ClassName "zoom-btn")
            , HP.style $ "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; background: white; opacity: " <> (if state.showLabels then "1.0" else "0.5") <> ";"
            ]
            [ HH.text "🏷️" ]
        , HH.button
            [ HE.onClick \_ -> SetFilter Nothing
            , HP.class_ (HH.ClassName "filter-btn")
            , HP.style $ "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; font-size: 13px; " <>
                (if isFilterAll state.filterNodeType
                 then "background: #3b82f6; color: white; border-color: #3b82f6;"
                 else "background: white; color: #374151;")
            ]
            [ HH.text "الكل" ]
        , HH.button
            [ HE.onClick \_ -> SetFilter (Just TargetRoot)
            , HP.class_ (HH.ClassName "filter-btn")
            , HP.style $ "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; font-size: 13px; " <>
                (if isFilterTargetRoot state.filterNodeType
                 then "background: #3b82f6; color: white; border-color: #3b82f6;"
                 else "background: white; color: #374151;")
            ]
            [ HH.text "الجذر" ]
        , HH.button
            [ HE.onClick \_ -> SetFilter (Just LetterNode)
            , HP.class_ (HH.ClassName "filter-btn")
            , HP.style $ "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; font-size: 13px; " <>
                (if isFilterLetterNode state.filterNodeType
                 then "background: #10b981; color: white; border-color: #10b981;"
                 else "background: white; color: #374151;")
            ]
            [ HH.text "الحروف" ]
        , HH.button
            [ HE.onClick \_ -> SetFilter (Just RelatedRoot)
            , HP.class_ (HH.ClassName "filter-btn")
            , HP.style $ "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; font-size: 13px; " <>
                (if isFilterRelatedRoot state.filterNodeType
                 then "background: #6366f1; color: white; border-color: #6366f1;"
                 else "background: white; color: #374151;")
            ]
            [ HH.text "مشابهة" ]
        , HH.button
            [ HE.onClick \_ -> SetFilter (Just SemanticConcept)
            , HP.class_ (HH.ClassName "filter-btn")
            , HP.style $ "padding: 6px 12px; height: 32px; border-radius: 6px; border: 1px solid #e5e7eb; cursor: pointer; font-size: 13px; " <>
                (if isFilterSemanticConcept state.filterNodeType
                 then "background: #f59e0b; color: white; border-color: #f59e0b;"
                 else "background: white; color: #374151;")
            ]
            [ HH.text "مفاهيم" ]
        ]
    , renderSvgGraph state graph
    ]

renderInfoPanel :: forall m. State -> { centerRoot :: RootText, graphNodes :: Array GraphNode, graphEdges :: Array GraphEdge, metadata :: String } -> H.ComponentHTML Action () m
renderInfoPanel state graph =
  case state.selectedNode of
    Nothing -> HH.text ""
    Just nodeId ->
      case findGraphNode nodeId graph.graphNodes of
        Nothing -> HH.text ""
        Just (GraphNode node) ->
          HH.div
            [ HP.class_ (HH.ClassName "graph-info-panel")
            , HP.style "background: #f9fafb; border: 1px solid #e5e7eb; border-radius: 8px; padding: 16px; margin-bottom: 12px; direction: rtl;"
            ]
            [ HH.div
                [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;" ]
                [ HH.h4
                    [ HP.style "margin: 0; font-size: 18px; font-weight: bold; color: #1f2937;" ]
                    [ HH.text node.arabicText ]
                , HH.button
                    [ HE.onClick \_ -> SelectNode ""
                    , HP.style "background: none; border: none; font-size: 20px; cursor: pointer; color: #6b7280;"
                    ]
                    [ HH.text "×" ]
                ]
            , HH.div
                [ HP.style "color: #6b7280; font-size: 14px; margin-bottom: 4px;" ]
                [ HH.text $ nodeTypeArabicLabel node.nodeType ]
            , case node.meaning of
                Just meaning ->
                  HH.div
                    [ HP.style "color: #374151; font-size: 14px; margin-top: 8px; padding: 8px; background: #fff; border-radius: 4px;" ]
                    [ HH.strong_ [ HH.text "المعنى: " ]
                    , HH.text meaning
                    ]
                Nothing -> HH.text ""
            , case node.occurrences of
                Just occ ->
                  HH.div
                    [ HP.style "color: #374151; font-size: 14px; margin-top: 8px; padding: 8px; background: #fff; border-radius: 4px;" ]
                    [ HH.strong_ [ HH.text "التكرار في القرآن: " ]
                    , HH.text $ show occ
                    ]
                Nothing -> HH.text ""
            ]

nodeTypeArabicLabel :: NodeType -> String
nodeTypeArabicLabel TargetRoot = "الجذر المستهدف"
nodeTypeArabicLabel LetterNode = "حرف"
nodeTypeArabicLabel RelatedRoot = "جذر مشابه"
nodeTypeArabicLabel SemanticConcept = "مفهوم دلالي"

renderSvgGraph :: forall m. State -> { centerRoot :: RootText, graphNodes :: Array GraphNode, graphEdges :: Array GraphEdge, metadata :: String } -> H.ComponentHTML Action () m
renderSvgGraph state graph =
  let
    viewBox = calculateViewBox state.zoom
    -- Filter nodes by type if a filter is set
    filteredNodes = Array.filter (nodeMatchesFilter state.filterNodeType) graph.graphNodes
  in
    SE.svg
      [ SA.viewBox viewBox.minX viewBox.minY viewBox.width viewBox.height
      , HP.class_ (HH.ClassName "etymology-svg")
      , SA.width 800.0
      , SA.height 600.0
      ]
      [ SE.g [ HP.class_ (HH.ClassName "edges") ]
          (map (renderSvgEdge graph.graphNodes) graph.graphEdges)
      , SE.g [ HP.class_ (HH.ClassName "nodes") ]
          (map (renderSvgNode state.selectedNode state.hoveredNode state.showLabels) filteredNodes)
      ]

-- Helper function to check if node matches filter
nodeMatchesFilter :: Maybe NodeType -> GraphNode -> Boolean
nodeMatchesFilter Nothing _ = true
nodeMatchesFilter (Just TargetRoot) (GraphNode n) = case n.nodeType of
  TargetRoot -> true
  _ -> false
nodeMatchesFilter (Just LetterNode) (GraphNode n) = case n.nodeType of
  LetterNode -> true
  _ -> false
nodeMatchesFilter (Just RelatedRoot) (GraphNode n) = case n.nodeType of
  RelatedRoot -> true
  _ -> false
nodeMatchesFilter (Just SemanticConcept) (GraphNode n) = case n.nodeType of
  SemanticConcept -> true
  _ -> false

-- Helper functions to check filter selection
isFilterAll :: Maybe NodeType -> Boolean
isFilterAll Nothing = true
isFilterAll _ = false

isFilterTargetRoot :: Maybe NodeType -> Boolean
isFilterTargetRoot (Just TargetRoot) = true
isFilterTargetRoot _ = false

isFilterLetterNode :: Maybe NodeType -> Boolean
isFilterLetterNode (Just LetterNode) = true
isFilterLetterNode _ = false

isFilterRelatedRoot :: Maybe NodeType -> Boolean
isFilterRelatedRoot (Just RelatedRoot) = true
isFilterRelatedRoot _ = false

isFilterSemanticConcept :: Maybe NodeType -> Boolean
isFilterSemanticConcept (Just SemanticConcept) = true
isFilterSemanticConcept _ = false

calculateViewBox :: Number -> ViewBox
calculateViewBox zoom =
  { minX: -400.0 / zoom
  , minY: -300.0 / zoom
  , width: 800.0 / zoom
  , height: 600.0 / zoom
  }

renderSvgEdge :: forall m. Array GraphNode -> GraphEdge -> H.ComponentHTML Action () m
renderSvgEdge nodes (GraphEdge edge) =
  case { from: findGraphNode edge.fromNodeId nodes, to: findGraphNode edge.toNodeId nodes } of
    { from: Just (GraphNode n1), to: Just (GraphNode n2) } ->
      let
        (NodePosition pos1) = n1.position
        (NodePosition pos2) = n2.position
      in
        SE.line
          [ SA.x1 pos1.posX
          , SA.y1 pos1.posY
          , SA.x2 pos2.posX
          , SA.y2 pos2.posY
          , SA.stroke (svgEdgeColor edge.edgeType)
          , SA.strokeWidth (svgEdgeWidth edge.edgeType)
          , HP.class_ (HH.ClassName $ "svg-edge edge-" <> edgeTypeClass edge.edgeType)
          ]
    _ -> SE.g [] []

renderSvgNode :: forall m. Maybe String -> Maybe String -> Boolean -> GraphNode -> H.ComponentHTML Action () m
renderSvgNode selected hovered showLabels graphNode@(GraphNode node) =
  let
    (NodePosition pos) = node.position
    isSelected = selected == Just node.nodeId
    isHovered = hovered == Just node.nodeId
    radius = svgNodeRadius graphNode
    baseColor = svgNodeColor node.nodeType
    -- Brighten color on hover
    color = if isHovered then svgNodeHoverColor node.nodeType else baseColor
  in
    SE.g
      [ HP.class_ (HH.ClassName $ "svg-node node-" <> nodeTypeClass node.nodeType <>
          (if isSelected then " selected" else "") <>
          (if isHovered then " hovered" else ""))
      , HE.onClick \_ -> SelectNode node.nodeId
      , HE.onMouseEnter \_ -> HoverNode node.nodeId
      , HE.onMouseLeave \_ -> UnhoverNode
      ]
      [ SE.circle
          [ SA.cx pos.posX
          , SA.cy pos.posY
          , SA.r (if isHovered then radius * 1.15 else radius)
          , SA.fill color
          , SA.stroke (if isSelected then Named "#000000" else color)
          , SA.strokeWidth (if isSelected then 3.0 else if isHovered then 2.5 else 1.5)
          ]
      , if showLabels || isHovered || isSelected
        then SE.g []
          [ -- Text background stroke for readability
            SE.text
              [ SA.x pos.posX
              , SA.y (pos.posY + 5.0)
              , SA.textAnchor SA.AnchorMiddle
              , SA.fontSize (FontSizeLength (Px (if isHovered then 16.0 else 14.0)))
              , SA.stroke (Named "#000000")
              , SA.strokeWidth 3.0
              , SA.fill (Named "#000000")
              , SA.fontWeight FWeightBold
              , HP.class_ (HH.ClassName "node-label-bg")
              ]
              [ HH.text node.arabicText ]
          , -- Actual white text on top
            SE.text
              [ SA.x pos.posX
              , SA.y (pos.posY + 5.0)
              , SA.textAnchor SA.AnchorMiddle
              , SA.fontSize (FontSizeLength (Px (if isHovered then 16.0 else 14.0)))
              , SA.fill (Named "#FFFFFF")
              , SA.fontWeight FWeightBold
              , HP.class_ (HH.ClassName "node-label")
              ]
              [ HH.text node.arabicText ]
          ]
        else SE.g [] []
      ]

findGraphNode :: String -> Array GraphNode -> Maybe GraphNode
findGraphNode nodeId nodes = Array.find (\(GraphNode n) -> n.nodeId == nodeId) nodes

svgNodeColor :: NodeType -> Color
svgNodeColor TargetRoot = Named "#3b82f6"
svgNodeColor LetterNode = Named "#10b981"
svgNodeColor RelatedRoot = Named "#6366f1"
svgNodeColor SemanticConcept = Named "#f59e0b"

svgNodeHoverColor :: NodeType -> Color
svgNodeHoverColor TargetRoot = Named "#60a5fa"
svgNodeHoverColor LetterNode = Named "#34d399"
svgNodeHoverColor RelatedRoot = Named "#818cf8"
svgNodeHoverColor SemanticConcept = Named "#fbbf24"

svgEdgeColor :: EdgeType -> Color
svgEdgeColor Contains = Named "#1f2937"
svgEdgeColor SharesLetter = Named "#9ca3af"
svgEdgeColor SemanticLink = Named "#6366f1"
svgEdgeColor EtymonPattern = Named "#f59e0b"

svgEdgeWidth :: EdgeType -> Number
svgEdgeWidth Contains = 2.0
svgEdgeWidth SharesLetter = 1.0
svgEdgeWidth SemanticLink = 1.5
svgEdgeWidth EtymonPattern = 1.5

-- Calculate node radius based on type and occurrences
svgNodeRadius :: GraphNode -> Number
svgNodeRadius (GraphNode node) = case node.nodeType of
  TargetRoot -> case node.occurrences of
    Just occ -> Number.log (toNumber occ + 1.0) * 8.0 + 20.0
    Nothing -> 25.0
  LetterNode -> 18.0
  RelatedRoot -> case node.occurrences of
    Just occ -> Number.log (toNumber occ + 1.0) * 6.0 + 15.0
    Nothing -> 15.0
  SemanticConcept -> 20.0

edgeTypeClass :: EdgeType -> String
edgeTypeClass Contains = "contains"
edgeTypeClass SharesLetter = "shares-letter"
edgeTypeClass SemanticLink = "semantic-link"
edgeTypeClass EtymonPattern = "etymon-pattern"

renderNode :: forall w i. GraphNode -> HH.HTML w i
renderNode (GraphNode node) =
  HH.li
    [ HP.class_ (HH.ClassName $ "graph-node node-" <> nodeTypeClass node.nodeType) ]
    [ HH.strong_ [ HH.text node.nodeId ]
    , HH.text " - "
    , HH.span
        [ HP.class_ (HH.ClassName "arabic-text") ]
        [ HH.text node.arabicText ]
    , case node.meaning of
        Just m -> HH.span_ [ HH.text $ " (" <> m <> ")" ]
        Nothing -> HH.text ""
    , case node.occurrences of
        Just n -> HH.span
            [ HP.class_ (HH.ClassName "occurrence-count") ]
            [ HH.text $ " [" <> show n <> " occurrences]" ]
        Nothing -> HH.text ""
    ]

renderEdge :: forall w i. GraphEdge -> HH.HTML w i
renderEdge (GraphEdge edge) =
  HH.li
    [ HP.class_ (HH.ClassName "graph-edge") ]
    [ HH.text $ edge.fromNodeId <> " → " <> edge.toNodeId
    , HH.span
        [ HP.class_ (HH.ClassName "edge-type") ]
        [ HH.text $ " (" <> edgeTypeLabel edge.edgeType <> ")" ]
    ]

edgeTypeLabel :: EdgeType -> String
edgeTypeLabel Contains = "Contains"
edgeTypeLabel SharesLetter = "Shares Letter"
edgeTypeLabel SemanticLink = "Semantic Link"
edgeTypeLabel EtymonPattern = "Etymon Pattern"

nodeTypeClass :: NodeType -> String
nodeTypeClass TargetRoot = "target-root"
nodeTypeClass LetterNode = "letter-node"
nodeTypeClass RelatedRoot = "related-root"
nodeTypeClass SemanticConcept = "semantic-concept"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load graph on initialization if we have a root
    state <- H.get
    when (state.currentRoot /= "") do
      H.modify_ _ { loading = true }
      result <- H.liftAff $ AX.get ResponseFormat.json ("/api/v1/etymology-graph/" <> state.currentRoot)
      case result of
        Left err -> H.modify_ _ { loading = false, error = Just $ "فشل تحميل الشبكة: " <> AX.printError err }
        Right response -> do
          case decodeJson response.body of
            Left decodeErr -> H.modify_ _ { loading = false, error = Just $ "فشل فك التشفير: " <> show decodeErr }
            Right graphData -> H.modify_ _ { loading = false, graphData = Just graphData, error = Nothing }

  Receive newRoot -> do
    state <- H.get
    -- Only load if the root has changed and is not empty
    when (newRoot /= state.currentRoot && newRoot /= "") do
      H.modify_ _ { currentRoot = newRoot, loading = true, error = Nothing }

      result <- H.liftAff $ AX.get ResponseFormat.json ("/api/v1/etymology-graph/" <> newRoot)

      case result of
        Left err -> do
          H.modify_ _
            { loading = false
            , error = Just $ "فشل تحميل الشبكة: " <> AX.printError err
            }

        Right response -> do
          case decodeJson response.body of
            Left decodeErr -> do
              H.modify_ _
                { loading = false
                , error = Just $ "فشل فك التشفير: " <> show decodeErr
                }

            Right graphData -> do
              H.modify_ _
                { loading = false
                , graphData = Just graphData
                , error = Nothing
                }

  ZoomIn -> do
    H.modify_ \s -> s { zoom = s.zoom * 1.2 }

  ZoomOut -> do
    H.modify_ \s -> s { zoom = s.zoom / 1.2 }

  ResetView -> do
    H.modify_ _ { zoom = 1.0, selectedNode = Nothing }

  SelectNode nodeId -> do
    H.modify_ _ { selectedNode = if nodeId == "" then Nothing else Just nodeId }

  HoverNode nodeId -> do
    H.modify_ _ { hoveredNode = Just nodeId }

  UnhoverNode -> do
    H.modify_ _ { hoveredNode = Nothing }

  ToggleLabels -> do
    H.modify_ \s -> s { showLabels = not s.showLabels }

  SetFilter nodeType -> do
    H.modify_ _ { filterNodeType = nodeType }

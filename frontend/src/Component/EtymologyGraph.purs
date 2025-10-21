module Component.EtymologyGraph where

import Prelude

import Data.Array (length, (!!), (..), foldl, find)
import Data.Array as Array
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Number (sqrt, pow)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)
import Web.UIEvent.MouseEvent as ME
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement, width, height)
import Web.HTML.HTMLCanvasElement as Canvas
import Graphics.Canvas (Context2D, getContext2D, setFillStyle, fillRect, clearRect, beginPath, moveTo, lineTo, stroke, arc, fill, setStrokeStyle, setLineWidth, setFont, fillText, measureText)

-- Domain types for graph nodes and edges
type Position = { x :: Number, y :: Number }
type Velocity = { vx :: Number, vy :: Number }

data NodeType = TargetRoot | LetterNode | RelatedRoot

derive instance eqNodeType :: Eq NodeType

type GraphNode =
  { nodeId :: String
  , nodeType :: NodeType
  , arabicText :: String
  , meaning :: Maybe String
  , occurrences :: Maybe Int
  , position :: Position
  , velocity :: Velocity
  , isDragging :: Boolean
  }

data EdgeType = Contains | SharesLetter

derive instance eqEdgeType :: Eq EdgeType

type GraphEdge =
  { edgeId :: String
  , edgeType :: EdgeType
  , fromNodeId :: String
  , toNodeId :: String
  , strength :: Number
  , label :: Maybe String
  }

type GraphData =
  { centerRoot :: String
  , nodes :: Array GraphNode
  , edges :: Array GraphEdge
  }

-- Component state
type State =
  { graphData :: Maybe GraphData
  , canvas :: Maybe HTMLCanvasElement
  , context :: Maybe Context2D
  , isDragging :: Boolean
  , draggedNode :: Maybe String
  , isPaused :: Boolean
  , zoom :: Number
  , pan :: Position
  , mousePos :: Position
  }

-- Component actions
data Action
  = Initialize
  | LoadGraph GraphData
  | Tick
  | MouseDown ME.MouseEvent
  | MouseMove ME.MouseEvent
  | MouseUp ME.MouseEvent
  | TogglePause
  | ResetView
  | ZoomIn
  | ZoomOut

-- Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall i. i -> State
initialState _ =
  { graphData: Nothing
  , canvas: Nothing
  , context: Nothing
  , isDragging: false
  , draggedNode: Nothing
  , isPaused: false
  , zoom: 1.0
  , pan: { x: 0.0, y: 0.0 }
  , mousePos: { x: 0.0, y: 0.0 }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ $ H.ClassName "etymology-graph-container" ]
    [ HH.div
        [ HP.class_ $ H.ClassName "graph-controls" ]
        [ HH.button
            [ HE.onClick \_ -> TogglePause
            , HP.class_ $ H.ClassName "control-btn"
            ]
            [ HH.text if state.isPaused then "▶ Resume" else "⏸ Pause" ]
        , HH.button
            [ HE.onClick \_ -> ZoomIn
            , HP.class_ $ H.ClassName "control-btn"
            ]
            [ HH.text "🔍+ Zoom In" ]
        , HH.button
            [ HE.onClick \_ -> ZoomOut
            , HP.class_ $ H.ClassName "control-btn"
            ]
            [ HH.text "🔍- Zoom Out" ]
        , HH.button
            [ HE.onClick \_ -> ResetView
            , HP.class_ $ H.ClassName "control-btn"
            ]
            [ HH.text "⟲ Reset View" ]
        , HH.div
            [ HP.class_ $ H.ClassName "zoom-level" ]
            [ HH.text $ "Zoom: " <> show (floor (state.zoom * 100.0)) <> "%" ]
        ]
    , HH.canvas
        [ HP.id "etymology-canvas"
        , HP.width 800
        , HP.height 600
        , HP.class_ $ H.ClassName "etymology-canvas"
        , HE.onMouseDown MouseDown
        , HE.onMouseMove MouseMove
        , HE.onMouseUp MouseUp
        ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Canvas will be initialized when graph data is loaded
    pure unit

  LoadGraph graphData -> do
    H.modify_ _ { graphData = Just graphData }
    -- Get canvas element and context
    pure unit

  Tick -> do
    state <- H.get
    when (not state.isPaused) do
      -- Update physics simulation
      let updated = updatePhysics state
      H.modify_ _ { graphData = updated.graphData }
      -- Render frame
      case { ctx: state.context, data: updated.graphData } of
        { ctx: Just ctx, data: Just gData } -> liftEffect $ renderFrame ctx gData state.zoom state.pan
        _ -> pure unit
      -- Schedule next frame
      -- liftEffect $ void $ requestAnimationFrame (const $ handleAction Tick)
      pure unit

  MouseDown event -> do
    state <- H.get
    let pos = getMousePosition event
    -- Check if clicking on a node
    case state.graphData of
      Just gData -> do
        let clicked = findNodeAt pos gData.nodes state.zoom state.pan
        case clicked of
          Just nodeId -> H.modify_ _ { isDragging = true, draggedNode = Just nodeId }
          Nothing -> pure unit
      Nothing -> pure unit

  MouseMove event -> do
    state <- H.get
    let pos = getMousePosition event
    H.modify_ _ { mousePos = pos }
    when state.isDragging do
      case { dragged: state.draggedNode, data: state.graphData } of
        { dragged: Just nodeId, data: Just gData } -> do
          -- Update dragged node position
          let updated = updateNodePosition nodeId pos gData state.zoom state.pan
          H.modify_ _ { graphData = Just updated }
        _ -> pure unit

  MouseUp _ -> do
    H.modify_ _ { isDragging = false, draggedNode = Nothing }

  TogglePause -> do
    H.modify_ \s -> s { isPaused = not s.isPaused }

  ResetView -> do
    H.modify_ _ { zoom = 1.0, pan = { x: 0.0, y: 0.0 } }

  ZoomIn -> do
    H.modify_ \s -> s { zoom = s.zoom * 1.2 }

  ZoomOut -> do
    H.modify_ \s -> s { zoom = s.zoom / 1.2 }

-- Physics simulation: Force-directed graph layout
updatePhysics :: State -> State
updatePhysics state = case state.graphData of
  Nothing -> state
  Just gData ->
    let
      -- Apply forces
      nodesWithForces = foldl applyRepulsion gData.nodes gData.nodes
      nodesWithAttraction = foldl (applyAttraction gData.edges) nodesWithForces gData.nodes
      -- Update positions
      updatedNodes = map updatePositionWithVelocity nodesWithAttraction
    in state { graphData = Just $ gData { nodes = updatedNodes } }
  where
    -- Repulsion force between all nodes
    applyRepulsion :: Array GraphNode -> GraphNode -> Array GraphNode
    applyRepulsion nodes node = map (applyRepulsionForce node) nodes

    applyRepulsionForce :: GraphNode -> GraphNode -> GraphNode
    applyRepulsionForce n1 n2 =
      if n1.nodeId == n2.nodeId then n2
      else
        let
          dx = n2.position.x - n1.position.x
          dy = n2.position.y - n1.position.y
          dist = sqrt (dx * dx + dy * dy)
          force = if dist > 0.0 then 1000.0 / (dist * dist) else 0.0
          fx = if dist > 0.0 then (dx / dist) * force else 0.0
          fy = if dist > 0.0 then (dy / dist) * force else 0.0
        in n2 { velocity = { vx: n2.velocity.vx + fx, vy: n2.velocity.vy + fy } }

    -- Attraction force along edges
    applyAttraction :: Array GraphEdge -> Array GraphNode -> GraphNode -> Array GraphNode
    applyAttraction edges nodes node = map (applyAttractionForce edges node) nodes

    applyAttractionForce :: Array GraphEdge -> GraphNode -> GraphNode -> GraphNode
    applyAttractionForce edges n1 n2 =
      let
        edge = findEdge edges n1.nodeId n2.nodeId
      in case edge of
        Nothing -> n2
        Just e ->
          let
            dx = n1.position.x - n2.position.x
            dy = n1.position.y - n2.position.y
            dist = sqrt (dx * dx + dy * dy)
            force = e.strength * dist * 0.01
            fx = if dist > 0.0 then (dx / dist) * force else 0.0
            fy = if dist > 0.0 then (dy / dist) * force else 0.0
          in n2 { velocity = { vx: n2.velocity.vx + fx, vy: n2.velocity.vy + fy } }

    updatePositionWithVelocity :: GraphNode -> GraphNode
    updatePositionWithVelocity node =
      if node.isDragging then node
      else
        let
          damping = 0.8
          newVx = node.velocity.vx * damping
          newVy = node.velocity.vy * damping
          newX = node.position.x + newVx
          newY = node.position.y + newVy
        in node
          { position = { x: newX, y: newY }
          , velocity = { vx: newVx, vy: newVy }
          }

    findEdge :: Array GraphEdge -> String -> String -> Maybe GraphEdge
    findEdge edges from to =
      find (\e -> (e.fromNodeId == from && e.toNodeId == to) || (e.fromNodeId == to && e.toNodeId == from)) edges

-- Canvas rendering
renderFrame :: Context2D -> GraphData -> Number -> Position -> Effect Unit
renderFrame ctx gData zoom pan = do
  -- Clear canvas
  clearRect ctx { x: 0.0, y: 0.0, width: 800.0, height: 600.0 }

  -- Draw edges
  void $ traverse (renderEdge ctx gData.nodes zoom pan) gData.edges

  -- Draw nodes
  void $ traverse (renderNode ctx zoom pan) gData.nodes

renderEdge :: Context2D -> Array GraphNode -> Number -> Position -> GraphEdge -> Effect Unit
renderEdge ctx nodes zoom pan edge = do
  case { from: findNode edge.fromNodeId nodes, to: findNode edge.toNodeId nodes } of
    { from: Just n1, to: Just n2 } -> do
      let
        x1 = (n1.position.x + pan.x) * zoom + 400.0
        y1 = (n1.position.y + pan.y) * zoom + 300.0
        x2 = (n2.position.x + pan.x) * zoom + 400.0
        y2 = (n2.position.y + pan.y) * zoom + 300.0
      setStrokeStyle ctx (edgeColor edge.edgeType)
      setLineWidth ctx (if edge.edgeType == Contains then 2.0 else 1.0)
      beginPath ctx
      moveTo ctx x1 y1
      lineTo ctx x2 y2
      stroke ctx
    _ -> pure unit

renderNode :: Context2D -> Number -> Position -> GraphNode -> Effect Unit
renderNode ctx zoom pan node = do
  let
    x = (node.position.x + pan.x) * zoom + 400.0
    y = (node.position.y + pan.y) * zoom + 300.0
    radius = nodeRadius node.nodeType * zoom

  -- Draw node circle
  setFillStyle ctx (nodeColor node.nodeType)
  beginPath ctx
  arc ctx { x, y, radius, start: 0.0, end: 6.28318530718, useCounterClockwise: false }
  fill ctx

  -- Draw text
  setFillStyle ctx "#000000"
  setFont ctx $ show (floor (14.0 * zoom)) <> "px 'Scheherazade New'"
  fillText ctx node.arabicText (x - 10.0) (y + 5.0)

-- Helper functions
nodeColor :: NodeType -> String
nodeColor = case _ of
  TargetRoot -> "#3b82f6"  -- Blue
  LetterNode -> "#10b981"  -- Green
  RelatedRoot -> "#6366f1" -- Indigo

edgeColor :: EdgeType -> String
edgeColor = case _ of
  Contains -> "#1f2937"    -- Dark gray
  SharesLetter -> "#9ca3af" -- Light gray

nodeRadius :: NodeType -> Number
nodeRadius = case _ of
  TargetRoot -> 20.0
  LetterNode -> 15.0
  RelatedRoot -> 12.0

findNode :: String -> Array GraphNode -> Maybe GraphNode
findNode nodeId nodes = find (\n -> n.nodeId == nodeId) nodes

getMousePosition :: ME.MouseEvent -> Position
getMousePosition event =
  { x: toNumber $ ME.clientX event
  , y: toNumber $ ME.clientY event
  }

findNodeAt :: Position -> Array GraphNode -> Number -> Position -> Maybe String
findNodeAt pos nodes zoom pan =
  let
    clicked = find (isNodeAt pos zoom pan) nodes
  in map _.nodeId clicked

isNodeAt :: Position -> Number -> Position -> GraphNode -> Boolean
isNodeAt pos zoom pan node =
  let
    nx = (node.position.x + pan.x) * zoom + 400.0
    ny = (node.position.y + pan.y) * zoom + 300.0
    dx = pos.x - nx
    dy = pos.y - ny
    dist = sqrt (dx * dx + dy * dy)
    radius = nodeRadius node.nodeType * zoom
  in dist <= radius

updateNodePosition :: String -> Position -> GraphData -> Number -> Position -> GraphData
updateNodePosition nodeId pos gData zoom pan =
  let
    updatedNodes = map updateNode gData.nodes
    updateNode node =
      if node.nodeId == nodeId
      then node
        { position =
            { x: (pos.x - 400.0) / zoom - pan.x
            , y: (pos.y - 300.0) / zoom - pan.y
            }
        , velocity = { vx: 0.0, vy: 0.0 }
        }
      else node
  in gData { nodes = updatedNodes }

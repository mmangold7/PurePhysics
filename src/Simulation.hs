module Simulation
  ( initialState
  , handleInput
  , updateState
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Data.Bifunctor

import Physics
import Types
import Draw

initialState :: State
initialState = State
  { particles = initialParticles
  , dragging = False
  , dragStart = (0, 0)
  , dragCurrent = (0, 0)
  , dragMass = initialDragMass
  , dragRadius = determineParticleRadius initialDragMass
  , viewScale = 1
  , viewTranslate = (0, 0)
  , panning = False
  , panStart = (0, 0)
  , viewStart = (0, 0)
  }

initialParticles :: [Particle]
initialParticles = 
  [ Particle (100, 200) (0, -1) 1e6
  , Particle (-100, -200) (0, 1) 1e6
  ]

initialDragMass :: Float
initialDragMass = 1e6

handleInput :: Event -> State -> State
handleInput event state = case event of
  (EventKey (MouseButton LeftButton) Down _ (x, y)) -> handleMouseDown x y state
  (EventMotion (x, y)) -> handleMouseMotion x y state
  (EventKey (MouseButton LeftButton) Up _ (x, y)) -> handleMouseUp x y state
  (EventKey (MouseButton RightButton) Down _ (x, y)) -> handleStartPanning x y state
  (EventKey (MouseButton RightButton) Up _ _) -> handleStopPanning state
  (EventKey (MouseButton WheelUp) Down _ _) -> handleZoomIn state
  (EventKey (MouseButton WheelDown) Down _ _) -> handleZoomOut state
  _ -> state

handleMouseDown :: Float -> Float -> State -> State
handleMouseDown x y state = 
  let worldPos = screenToWorld state (x, y)
  in traceShow ("Mouse Down at", (x, y), "World Position", worldPos) $
     state { dragging = True, dragStart = worldPos, dragCurrent = worldPos, dragMass = initialDragMass }

handleMouseMotion :: Float -> Float -> State -> State
handleMouseMotion x y state
  | dragging state = 
      let worldPos = screenToWorld state (x, y)
      in traceShow ("Mouse Motion at", (x, y), "World Position", worldPos) $
         state { dragCurrent = worldPos }
  | panning state = 
      let newTranslate = bimap
            (((x - fst (panStart state)) / viewScale state) +)
            (((y - snd (panStart state)) / viewScale state) +)
            (viewStart state)
      in traceShow ("Panning to", newTranslate) $
         state { viewTranslate = newTranslate }
  | otherwise = state

handleMouseUp :: Float -> Float -> State -> State
handleMouseUp x y state =
  let (x0, y0) = dragStart state
      (xf, yf) = screenToWorld state (x, y)
      prospectiveVelocity = ((xf - x0) * 0.1, (yf - y0) * 0.1) -- Scale factor to adjust velocity
  in traceShow ("Mouse Up at", (x, y), "World Position", (xf, yf), "Velocity", prospectiveVelocity) $
     state { dragging = False, particles = Particle (dragStart state) prospectiveVelocity (dragMass state) : particles state }

handleStartPanning :: Float -> Float -> State -> State
handleStartPanning x y state =
  traceShow ("Start Panning at", (x, y)) $
  state { panning = True, panStart = (x, y), viewStart = viewTranslate state }

handleStopPanning :: State -> State
handleStopPanning state =
  traceShow "Stop Panning" $
  state { panning = False }

handleZoomIn :: State -> State
handleZoomIn state =
  let newScale = viewScale state * 1.1
      newTranslate = (fst (viewTranslate state) * 1.1, snd (viewTranslate state) * 1.1)
  in traceShow ("Zoom In", "New Scale", newScale, "New Translate", newTranslate) $
     state { viewScale = newScale, viewTranslate = newTranslate }

handleZoomOut :: State -> State
handleZoomOut state =
  let newScale = viewScale state / 1.1
      newTranslate = (fst (viewTranslate state) / 1.1, snd (viewTranslate state) / 1.1)
  in traceShow ("Zoom Out", "New Scale", newScale, "New Translate", newTranslate) $
     state { viewScale = newScale, viewTranslate = newTranslate }

updateState :: Float -> State -> State
updateState _ state =
  if dragging state
  then let newMass = if withinRadius (dragStart state) (dragCurrent state) (5 + dragRadius state)
                     then dragMass state * 1.25
                     else dragMass state
       in state { dragMass = newMass, particles = map (updateParticle (particles state)) (particles state) }
  else state { particles = map (updateParticle (particles state)) (particles state) }

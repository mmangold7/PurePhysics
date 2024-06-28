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
  { particles =
      [ Particle (100, 200) (0, -1) 1e6
      , Particle (-100, -200) (0, 1) 1e6
      ]
  , dragging = False
  , dragStart = (0, 0)
  , dragCurrent = (0, 0)
  , dragMass = 1e6
  , dragRadius = determineParticleRadius 1e6
  , viewScale = 1
  , viewTranslate = (0, 0)
  , panning = False
  , panStart = (0, 0)
  , viewStart = (0, 0)
  }

handleInput :: Event -> State -> State
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
  let worldPos = screenToWorld state (x, y)
  in traceShow ("Mouse Down at", (x, y), "World Position", worldPos) $
     state
      { dragging = True
      , dragStart = worldPos
      , dragCurrent = worldPos
      , dragMass = 1e6
      }
handleInput (EventMotion (x, y)) state
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
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) state =
  let (x0, y0) = dragStart state
      (xf, yf) = screenToWorld state (x, y)
      prospectiveVelocity = ((xf - x0) * 0.1, (yf - y0) * 0.1) -- Scale factor to adjust velocity
  in traceShow ("Mouse Up at", (x, y), "World Position", (xf, yf), "Velocity", prospectiveVelocity) $
     state
     { dragging = False
     , particles = Particle (dragStart state) prospectiveVelocity (dragMass state) : particles state
     }
handleInput (EventKey (MouseButton RightButton) Down _ (x, y)) state =
  traceShow ("Start Panning at", (x, y)) $
  state
  { panning = True
  , panStart = (x, y)
  , viewStart = viewTranslate state
  }
handleInput (EventKey (MouseButton RightButton) Up _ _) state =
  traceShow "Stop Panning" $
  state
  { panning = False
  }
handleInput (EventKey (MouseButton WheelUp) Down _ _) state =
  let newScale = viewScale state * 1.1
      newTranslate = (fst (viewTranslate state) * 1.1, snd (viewTranslate state) * 1.1)
  in traceShow ("Zoom In", "New Scale", newScale, "New Translate", newTranslate) $
     state
  { viewScale = newScale
  , viewTranslate = newTranslate
  }
handleInput (EventKey (MouseButton WheelDown) Down _ _) state =
  let newScale = viewScale state / 1.1
      newTranslate = (fst (viewTranslate state) / 1.1, snd (viewTranslate state) / 1.1)
  in traceShow ("Zoom Out", "New Scale", newScale, "New Translate", newTranslate) $
     state
  { viewScale = newScale
  , viewTranslate = newTranslate
  }
handleInput _ state = state

updateState :: Float -> State -> State
updateState _ state =
  if dragging state
  then let newMass = if withinRadius (dragStart state) (dragCurrent state) (5 + dragRadius state)
                     then dragMass state * 1.25
                     else dragMass state
       in state { dragMass = newMass, particles = map (updateParticle (particles state)) (particles state) }
  else state { particles = map (updateParticle (particles state)) (particles state) }

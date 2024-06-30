{-# LANGUAGE RecordWildCards #-}

module Simulation
  ( initialState
  , initialParticles
  , initialDragMass
  , handleInput
  , handleMouseDown
  , handleMouseMotion
  , handleMouseUp
  , handleStartPanning
  , handleStopPanning
  , handleZoomIn
  , handleZoomOut
  , updateState
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Data.Bifunctor
import Physics
import Types
import Draw

centralMass :: Float
centralMass = 1e8 -- Central massive particle's mass

ringRadius :: Float
ringRadius = 200 -- Radius of the ring

numParticles :: Int
numParticles = 10 -- Number of particles in the ring

particleMass :: Float
particleMass = 1e6 -- Mass of each particle in the ring

initialParticles :: [Particle]
initialParticles = centralParticle : ringParticles
  where
    centralParticle = Particle (0, 0) (0, 0) centralMass
    ringParticles = [Particle (x, y) (vx, vy) particleMass | i <- [0..numParticles-1], 
                     let angle = 2 * pi * fromIntegral i / fromIntegral numParticles
                         x = ringRadius * cos angle
                         y = ringRadius * sin angle
                         vx = -sqrt (gravityConstant * centralMass / ringRadius) * sin angle
                         vy = sqrt (gravityConstant * centralMass / ringRadius) * cos angle]

initialDragMass :: Float
initialDragMass = 1e6

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
  , showDebug = False
  , drawMode = Filled
  , buttonPos = (-350, 230)
  , buttonSize = (120, 40)
  , drawModeButtonPos = (-350, 180)
  , drawModeButtonSize = (180, 40)
  , timeStep = 0.0
  , sliderPos = (-350, -250)
  , sliderSize = (400, 20) -- Double the length of the slider
  , sliderValue = 0.5 -- Start the slider in the middle (0 position)
  , isSliderActive = False
  , windowSize = (800, 600) -- Initial window size
  }

handleInput :: Event -> State -> State
handleInput event state = case event of
  (EventKey (MouseButton LeftButton) Down _ (x, y)) -> handleMouseDown x y state
  (EventMotion (x, y)) -> handleMouseMotion x y state
  (EventKey (MouseButton LeftButton) Up _ (x, y)) -> handleMouseUp x y state
  (EventKey (MouseButton RightButton) Down _ (x, y)) -> handleStartPanning x y state
  (EventKey (MouseButton RightButton) Up _ _) -> handleStopPanning state
  (EventKey (MouseButton WheelUp) Down _ _) -> handleZoomIn state
  (EventKey (MouseButton WheelDown) Down _ _) -> handleZoomOut state
  (EventKey (Char 'd') Down _ _) -> state { showDebug = not (showDebug state) }
  (EventResize (w, h)) -> state { windowSize = (w, h) }
  _ -> state

handleMouseDown :: Float -> Float -> State -> State
handleMouseDown x y state
  | withinButton (x, y) (adjustedButtonPos state) (buttonSize state) = state { showDebug = not (showDebug state) }
  | withinButton (x, y) (adjustedDrawModeButtonPos state) (drawModeButtonSize state) = state { drawMode = nextDrawMode (drawMode state) }
  | withinSlider (x, y) (adjustedSliderPos state) (sliderSize state) = state { sliderValue = (x - fst (adjustedSliderPos state)) / fst (sliderSize state), timeStep = -0.9 + ((x - fst (adjustedSliderPos state)) / fst (sliderSize state)) * 1.8, isSliderActive = True }
  | otherwise =
      let worldPos = screenToWorld state (x, y)
      in state { dragging = True, dragStart = worldPos, dragCurrent = worldPos, dragMass = initialDragMass }

handleMouseMotion :: Float -> Float -> State -> State
handleMouseMotion x y state
  | dragging state = 
      let worldPos = screenToWorld state (x, y)
      in state { dragCurrent = worldPos }
  | panning state = 
      let newTranslate = bimap
            (((x - fst (panStart state)) / viewScale state) +)
            (((y - snd (panStart state)) / viewScale state) +)
            (viewStart state)
      in state { viewTranslate = newTranslate }
  | isSliderActive state = 
      let (sx, _) = adjustedSliderPos state
          (sw, _) = sliderSize state
          newValue = (x - sx) / sw
          clampedValue = max 0 (min 1 newValue) -- Ensure the value stays within 0 and 1
          timeStepValue = -0.9 + clampedValue * 1.8 -- Map the slider value to range -0.9 to 0.9
      in state { sliderValue = clampedValue, timeStep = timeStepValue }
  | otherwise = state

handleMouseUp :: Float -> Float -> State -> State
handleMouseUp x y state =
  if not (withinButton (x, y) (adjustedButtonPos state) (buttonSize state))
     && not (withinButton (x, y) (adjustedDrawModeButtonPos state) (drawModeButtonSize state))
     && not (withinSlider (x, y) (adjustedSliderPos state) (sliderSize state))
  then let (x0, y0) = dragStart state
           (xf, yf) = screenToWorld state (x, y)
           prospectiveVelocity = ((xf - x0) * 0.1, (yf - y0) * 0.1) -- Scale factor to adjust velocity
       in state { dragging = False, particles = Particle (dragStart state) prospectiveVelocity (dragMass state) : particles state, isSliderActive = False }
  else state { dragging = False, isSliderActive = False }

handleStartPanning :: Float -> Float -> State -> State
handleStartPanning x y state =
  state { panning = True, panStart = (x, y), viewStart = viewTranslate state }

handleStopPanning :: State -> State
handleStopPanning state = state { panning = False }

handleZoomIn :: State -> State
handleZoomIn state =
  let newScale = viewScale state * 1.1
      newTranslate = (fst (viewTranslate state) * 1.1, snd (viewTranslate state) * 1.1)
  in state { viewScale = newScale, viewTranslate = newTranslate }

handleZoomOut :: State -> State
handleZoomOut state =
  let newScale = viewScale state / 1.1
      newTranslate = (fst (viewTranslate state) / 1.1, snd (viewTranslate state) / 1.1)
  in state { viewScale = newScale, viewTranslate = newTranslate }

updateState :: Float -> State -> State
updateState _ state =
  if dragging state
  then let newMass = if withinRadius (dragStart state) (dragCurrent state) (5 + dragRadius state)
                     then dragMass state * 1.25
                     else dragMass state
       in state { dragMass = newMass, particles = map (updateParticle (particles state) (timeStep state)) (particles state) }
  else state { particles = map (updateParticle (particles state) (timeStep state)) (particles state) }

withinButton :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
withinButton (x, y) (bx, by) (bw, bh) = 
  x >= bx && x <= bx + bw && y >= by && y <= by + bh

withinSlider :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
withinSlider (x, y) (sx, sy) (sw, sh) = 
  x >= sx && x <= sx + sw && y >= sy && y <= sy + sh

nextDrawMode :: DrawingMode -> DrawingMode
nextDrawMode Filled = Outline
nextDrawMode Outline = Crosshair
nextDrawMode Crosshair = Filled

module Simulation
  ( initialState
  , initialParticles
  , initialMultipleSystems
  , initialRealisticSolarSystem
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
import Data.Bifunctor
import Physics
import Types
import Draw

initialDragMass :: Float
initialDragMass = 1e6

centralMass :: Float
centralMass = 1e8

ringRadius :: Float
ringRadius = 200

numParticles :: Int
numParticles = 10

particleMass :: Float
particleMass = 1e6

initialParticles :: [Particle]
initialParticles = centralParticle : ringParticles
  where
    centralParticle = Particle (0, 0) (0, 0) centralMass (determineParticleColor centralMass)
    ringParticles = [Particle (x, y) (vx, vy) particleMass (determineParticleColor particleMass) | i <- [0..numParticles-1], 
                     let angle = 2 * pi * fromIntegral i / fromIntegral numParticles
                         x = ringRadius * cos angle
                         y = ringRadius * sin angle
                         vx =  - (sqrt (gravityConstant * centralMass / ringRadius) * sin angle)
                         vy = sqrt (gravityConstant * centralMass / ringRadius) * cos angle]

initialMultipleSystems :: [Particle]
initialMultipleSystems = concatMap createSystem [(0, 0), (1000, 1000), (-1000, -1000)]
  where
    createSystem (cx, cy) = centralStar : planets
      where
        centralStar = Particle (cx, cy) (0, 0) centralMass (determineParticleColor centralMass)
        planets = [Particle (x + cx, y + cy) (vx, vy) particleMass (determineParticleColor particleMass) | i <- [1..5],
                   let angle = 2 * pi * fromIntegral i / 5
                       inputDistance = 200 + fromIntegral (i * 50)
                       x = inputDistance * cos angle
                       y = inputDistance * sin angle
                       vx = - (sqrt (gravityConstant * centralMass / inputDistance) * sin angle)
                       vy = sqrt (gravityConstant * centralMass / inputDistance) * cos angle]

solarMasses :: [(String, Float)]
solarMasses =
  [ ("Sun", 1.9885e30)
  , ("Mercury", 3.3011e23)
  , ("Venus", 4.8675e24)
  , ("Earth", 5.97237e24)
  , ("Mars", 6.4171e23)
  , ("Jupiter", 1.8982e27)
  , ("Saturn", 5.6834e26)
  , ("Uranus", 8.6810e25)
  , ("Neptune", 1.02413e26)
  , ("Pluto", 1.30900e22)
  ]

-- Orbital Radii (in meters)
orbitalRadii :: [(String, Float)]
orbitalRadii =
  [ ("Mercury", 57.91e9)
  , ("Venus", 108.21e9)
  , ("Earth", 149.60e9)
  , ("Mars", 227.92e9)
  , ("Jupiter", 778.57e9)
  , ("Saturn", 1.43e12)
  , ("Uranus", 2.87e12)
  , ("Neptune", 4.50e12)
  , ("Pluto", 5.91e12)
  ]

-- Velocities (calculated for circular orbits)
orbitalVelocities :: [(String, Float)]
orbitalVelocities =
  [ ("Mercury", 47.87e3)
  , ("Venus", 35.02e3)
  , ("Earth", 29.78e3)
  , ("Mars", 24.077e3)
  , ("Jupiter", 13.07e3)
  , ("Saturn", 9.69e3)
  , ("Uranus", 6.81e3)
  , ("Neptune", 5.43e3)
  , ("Pluto", 4.74e3)
  ]

-- Initial Particles for Realistic Solar System
initialRealisticSolarSystem :: [Particle]
initialRealisticSolarSystem = 
  let sun = Particle (0, 0) (0, 0) (massOf "Sun") white
      planets = zipWith createPlanet orbitalRadii orbitalVelocities
  in sun : planets
  where
    createPlanet (name, radius) (_, velocity) =
      let angle = 0 -- All planets start aligned for simplicity
      in Particle (radius * cos angle, radius * sin angle)
                  (0, velocity) -- Velocity perpendicular to radius for circular orbit
                  (massOf name)
                  (colorOf name)
    massOf name = snd $ head $ filter (\(n, _) -> n == name) solarMasses
    colorOf name = case name of
      "Mercury" -> greyN 0.5
      "Venus" -> makeColor 0.8 0.6 0.4 1.0
      "Earth" -> blue
      "Mars" -> red
      "Jupiter" -> makeColor 0.8 0.5 0.2 1.0
      "Saturn" -> makeColor 0.9 0.8 0.5 1.0
      "Uranus" -> cyan
      "Neptune" -> makeColor 0.4 0.2 0.8 1.0
      "Pluto" -> makeColor 0.6 0.6 0.6 1.0
      _ -> white

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
  , colorMode = MassBased
  , arrowSizeMode = Constant
  , startingStateMode = SingleSystem
  , buttonPos = (-350, 230)
  , buttonSize = (120, 40)
  , drawModeButtonPos = (-350, 180)
  , drawModeButtonSize = (180, 40)
  , colorModeButtonPos = (-350, 130)
  , colorModeButtonSize = (180, 40)
  , arrowSizeButtonPos = (-350, 80)
  , arrowSizeButtonSize = (180, 40)
  , stateButtonPos = (-350, 30)
  , stateButtonSize = (180, 40)
  , timeStep = 0.0
  , sliderPos = (-350, -250)
  , sliderSize = (400, 20)
  , sliderValue = 0.5
  , isSliderActive = False
  , windowSize = (800, 600)
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
  | withinButton (x, y) (adjustedColorModeButtonPos state) (colorModeButtonSize state) = state { colorMode = nextColorMode (colorMode state) }
  | withinButton (x, y) (adjustedArrowSizeButtonPos state) (arrowSizeButtonSize state) = state { arrowSizeMode = nextArrowSizeMode (arrowSizeMode state) }
  | withinButton (x, y) (adjustedStartingStateButtonPos state) (stateButtonSize state) = handleStartingStateChange state
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
          clampedValue = max 0 (min 1 newValue)
          timeStepValue = -0.9 + clampedValue * 1.8
      in state { sliderValue = clampedValue, timeStep = timeStepValue }
  | otherwise = state

handleMouseUp :: Float -> Float -> State -> State
handleMouseUp x y state =
  if not (withinButton (x, y) (adjustedButtonPos state) (buttonSize state))
     && not (withinButton (x, y) (adjustedDrawModeButtonPos state) (drawModeButtonSize state))
     && not (withinButton (x, y) (adjustedColorModeButtonPos state) (colorModeButtonSize state))
     && not (withinButton (x, y) (adjustedArrowSizeButtonPos state) (arrowSizeButtonSize state))
     && not (withinButton (x, y) (adjustedStartingStateButtonPos state) (stateButtonSize state))
     && not (withinSlider (x, y) (adjustedSliderPos state) (sliderSize state))
  then let (x0, y0) = dragStart state
           (xf, yf) = screenToWorld state (x, y)
           prospectiveVelocity = ((xf - x0) * 0.1, (yf - y0) * 0.1)
       in state { dragging = False, particles = Particle (dragStart state) prospectiveVelocity (dragMass state) (determineParticleColor (dragMass state)) : particles state, isSliderActive = False }
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

handleStartingStateChange :: State -> State
handleStartingStateChange state = 
  case startingStateMode state of
    SingleSystem -> state { startingStateMode = MultipleSystems, particles = initialMultipleSystems }
    MultipleSystems -> state { startingStateMode = RealisticSolarSystem, particles = initialRealisticSolarSystem }
    RealisticSolarSystem -> state { startingStateMode = Types.Blank, particles = [] }
    Types.Blank -> state { startingStateMode = SingleSystem, particles = initialParticles }

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
  x >= sx && x <= sx + sw && y <= sy + sh

nextDrawMode :: DrawingMode -> DrawingMode
nextDrawMode Filled = Outline
nextDrawMode Outline = Crosshair
nextDrawMode Crosshair = Filled

nextColorMode :: ColorMode -> ColorMode
nextColorMode MassBased = Random
nextColorMode Random = MassBased

nextArrowSizeMode :: ArrowSizeMode -> ArrowSizeMode
nextArrowSizeMode Constant = Scaled
nextArrowSizeMode Scaled = Constant

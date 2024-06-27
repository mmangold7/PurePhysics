{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Data types for the particles
data Particle = Particle
  { position :: (Float, Float)
  , velocity :: (Float, Float)
  , mass     :: Float
  } deriving (Eq)

-- Simulation state including the particles and dragging state
data State = State
  { particles    :: [Particle]
  , dragging     :: Bool
  , dragStart    :: (Float, Float)
  , dragCurrent  :: (Float, Float)
  } deriving (Eq)

-- Simulation constants
gravityConstant :: Float
gravityConstant = 6.67430e-11

-- Number of particles and time step
timeStep :: Float
timeStep = 0.1

-- Initial state of the particles
initialState :: State
initialState = State
  { particles = 
      [ Particle (100, 200) (0, -1) 1e6
      , Particle (-100, -200) (0, 1) 1e6
      ]
  , dragging = False
  , dragStart = (0, 0)
  , dragCurrent = (0, 0)
  }

-- Main function
main :: IO ()
main = play
  (InWindow "2D Gravity Simulation" (800, 600) (100, 100))
  black
  60
  initialState
  drawState
  handleInput
  updateState

-- Function to draw the state
drawState :: State -> Picture
drawState State{..} = Pictures (map drawParticle particles ++ dragPicture)
  where
    dragPicture = if dragging
                  then [drawArrow dragStart dragCurrent]
                  else []

drawParticle :: Particle -> Picture
drawParticle Particle{..} = Translate x y (Color white (circleSolid 5))
  where
    (x, y) = position

drawArrow :: (Float, Float) -> (Float, Float) -> Picture
drawArrow (x1, y1) (x2, y2) = Color red $ Pictures
  [ Line [(x1, y1), (x2, y2)]
  , Translate x2 y2 $ Rotate (angle x1 y1 x2 y2) $ Polygon [(0, 0), (-5, -10), (5, -10)]
  ]
  where
    angle x1 y1 x2 y2 = 180 * atan2 (y2 - y1) (x2 - x1) / pi

-- Function to handle input events
handleInput :: Event -> State -> State
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) state = state
  { dragging = True
  , dragStart = (x, y)
  , dragCurrent = (x, y)
  }
handleInput (EventMotion (x, y)) state
  | dragging state = state { dragCurrent = (x, y) }
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) state = state
  { dragging = False
  , particles = Particle (x, y) velocity 1e6 : particles state
  }
  where
    (x0, y0) = dragStart state
    velocity = ((x - x0) * 0.1, (y - y0) * 0.1) -- Scale factor to adjust velocity
handleInput _ state = state

-- Function to update the state
updateState :: Float -> State -> State
updateState _ state = state { particles = map (updateParticle (particles state)) (particles state) }

updateParticle :: [Particle] -> Particle -> Particle
updateParticle particles p@Particle{..} = p
  { position = (x + vx * timeStep, y + vy * timeStep)
  , velocity = (vx + ax * timeStep, vy + ay * timeStep)
  }
  where
    (x, y) = position
    (vx, vy) = velocity
    (ax, ay) = acceleration particles p

acceleration :: [Particle] -> Particle -> (Float, Float)
acceleration particles p = (sum ax, sum ay)
  where
    (ax, ay) = unzip [gravitationalForce p p' | p' <- particles, p' /= p]

gravitationalForce :: Particle -> Particle -> (Float, Float)
gravitationalForce Particle{..} Particle{position = (x', y'), mass = m'} =
  (fx / mass, fy / mass)
  where
    (x, y) = position
    dx = x' - x
    dy = y' - y
    distance = sqrt (dx * dx + dy * dy)
    force = gravityConstant * mass * m' / (distance * distance)
    fx = force * dx / distance
    fy = force * dy / distance

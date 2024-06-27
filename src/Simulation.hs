{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Particle

-- Simulation state including the particles and dragging state
data State = State
  { particles    :: [Particle]
  , dragging     :: Bool
  , dragStart    :: (Float, Float)
  , dragCurrent  :: (Float, Float)
  , dragMass     :: Float
  } deriving (Eq, Show)

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
  , dragMass = 1e6
  }

-- Function to draw the state
drawState :: State -> Picture
drawState state@State{..} = Pictures (drawParticles particles ++ dragPicture ++ drawDebugInfo state)
  where
    dragPicture = if dragging
                  then [drawArrow dragStart dragCurrent, drawDragMass dragStart dragMass]
                  else []

drawParticles :: [Particle] -> [Picture]
drawParticles particles = map drawParticle particles

drawArrow :: (Float, Float) -> (Float, Float) -> Picture
drawArrow (x1, y1) (x2, y2) = Color red $ Pictures
  [ Line [(x1, y1), (x2, y2)]
  , Translate x2 y2 $ Rotate (angle x1 y1 x2 y2) $ Polygon [(0, 0), (-10, 5), (-10, -5)]
  ]
  where
    angle x1 y1 x2 y2 = 180 * atan2 (y1 - y2) (x2 - x1) / pi

drawDragMass :: (Float, Float) -> Float -> Picture
drawDragMass (x, y) mass = Translate x y (Color yellow (circleSolid (radius mass)))

drawDebugInfo :: State -> [Picture]
drawDebugInfo State{..} = zipWith drawParticleInfo particles [0..]

drawParticleInfo :: Particle -> Int -> Picture
drawParticleInfo Particle{..} index = Translate (-390) (290 - 20 * fromIntegral index) $ Scale 0.1 0.1 $ Color white $ Text info
  where
    info = "Particle " ++ show index ++ ": Pos=(" ++ show (round x) ++ "," ++ show (round y) ++ "), Vel=(" ++ show (round vx) ++ "," ++ show (round vy) ++ "), Mass=" ++ show (round mass)
    (x, y) = position
    (vx, vy) = velocity

-- Function to handle input events
handleInput :: Event -> State -> State
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) state = state
  { dragging = True
  , dragStart = (x, y)
  , dragCurrent = (x, y)
  , dragMass = 1e6
  }
handleInput (EventMotion (x, y)) state
  | dragging state = state { dragCurrent = (x, y) }
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) state = state
  { dragging = False
  , particles = Particle (dragStart state) velocity (dragMass state) : particles state
  }
  where
    (x0, y0) = dragStart state
    velocity = ((x - x0) * 0.1, (y - y0) * 0.1) -- Scale factor to adjust velocity
handleInput _ state = state

-- Function to update the state
updateState :: Float -> State -> State
updateState _ state =
  if dragging state
  then let newMass = if withinRadius (dragStart state) (dragCurrent state) (5 + radius (dragMass state))
                     then dragMass state + 1e7  -- Further increase mass increment rate
                     else dragMass state
       in state { dragMass = newMass, particles = map (updateParticle (particles state)) (particles state) }
  else state { particles = map (updateParticle (particles state)) (particles state) }

withinRadius :: (Float, Float) -> (Float, Float) -> Float -> Bool
withinRadius (x1, y1) (x2, y2) r = distance (x1, y1) (x2, y2) <= r

updateParticle :: [Particle] -> Particle -> Particle
updateParticle particles p@Particle{..} =
  if isNaN x || isNaN y || isNaN vx || isNaN vy || isNaN ax || isNaN ay
  then p
  else p
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
  if distance == 0
  then (0, 0)
  else (fx / mass, fy / mass)
  where
    (x, y) = position
    dx = x' - x
    dy = y' - y
    distance = sqrt (dx * dx + dy * dy)
    force = gravityConstant * mass * m' / (distance * distance)
    fx = force * dx / distance
    fy = force * dy / distance

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

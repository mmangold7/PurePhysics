{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Particle

-- Simulation state including the particles, dragging state, and view state
data State = State
  { particles    :: [Particle]
  , dragging     :: Bool
  , dragStart    :: (Float, Float)
  , dragCurrent  :: (Float, Float)
  , dragMass     :: Float
  , viewScale    :: Float
  , viewTranslate :: (Float, Float)
  , panning      :: Bool
  , panStart     :: (Float, Float)
  , viewStart    :: (Float, Float)
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
  , viewScale = 1
  , viewTranslate = (0, 0)
  , panning = False
  , panStart = (0, 0)
  , viewStart = (0, 0)
  }

-- Function to draw the state
drawState :: State -> Picture
drawState state@State{..} = Pictures [view, hud]
  where
    view = Scale viewScale viewScale (Translate (fst viewTranslate) (snd viewTranslate) (Pictures (drawParticles particles ++ dragPicture)))
    hud = Translate (-390) 290 $ Scale 0.1 0.1 $ Color white $ Text (unlines (map showParticleInfo (zip particles [0..])))
    dragPicture = if dragging
                  then [drawArrow (worldToScreen state dragStart) (worldToScreen state dragCurrent), drawDragMass (worldToScreen state dragStart) dragMass]
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
drawDragMass (x, y) mass = Translate x y (Color (particleColor mass) (circleSolid (radius mass)))

showParticleInfo :: (Particle, Int) -> String
showParticleInfo (Particle{..}, index) = "Particle " ++ show index ++ ": Pos=(" ++ show (round x) ++ "," ++ show (round y) ++ "), Vel=(" ++ show (round vx) ++ "," ++ show (round vy) ++ "), Mass=" ++ show (round mass)
  where
    (x, y) = position
    (vx, vy) = velocity

-- Function to handle input events
handleInput :: Event -> State -> State
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) state = state
  { dragging = True
  , dragStart = screenToWorld state (x, y)
  , dragCurrent = screenToWorld state (x, y)
  , dragMass = 1e6
  }
handleInput (EventMotion (x, y)) state
  | dragging state = state { dragCurrent = screenToWorld state (x, y) }
  | panning state = state { viewTranslate = (viewStart state `plus` ((x, y) `minus` panStart state)) }
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) state = 
  let (x0, y0) = dragStart state
      (xf, yf) = screenToWorld state (x, y)
      velocity = ((xf - x0) * 0.1, (yf - y0) * 0.1) -- Scale factor to adjust velocity
  in state
     { dragging = False
     , particles = Particle (dragStart state) velocity (dragMass state) : particles state
     }
handleInput (EventKey (MouseButton RightButton) Down _ (x, y)) state = state
  { panning = True
  , panStart = (x, y)
  , viewStart = viewTranslate state
  }
handleInput (EventKey (MouseButton RightButton) Up _ _) state = state
  { panning = False
  }
handleInput (EventKey (MouseButton WheelUp) Down _ _) state = state
  { viewScale = viewScale state * 1.1
  }
handleInput (EventKey (MouseButton WheelDown) Down _ _) state = state
  { viewScale = viewScale state / 1.1
  }
handleInput _ state = state

-- Function to update the state
updateState :: Float -> State -> State
updateState _ state =
  if dragging state
  then let newMass = if withinRadius (dragStart state) (dragCurrent state) (5 + radius (dragMass state))
                     then dragMass state * 1.05  -- Increase mass exponentially at a faster rate
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

screenToWorld :: State -> (Float, Float) -> (Float, Float)
screenToWorld State{..} (sx, sy) = ((sx - tx) / s, (sy - ty) / s)
  where
    (tx, ty) = viewTranslate
    s = viewScale

worldToScreen :: State -> (Float, Float) -> (Float, Float)
worldToScreen State{..} (wx, wy) = (wx * s + tx, wy * s + ty)
  where
    (tx, ty) = viewTranslate
    s = viewScale

plus :: (Float, Float) -> (Float, Float) -> (Float, Float)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minus :: (Float, Float) -> (Float, Float) -> (Float, Float)
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

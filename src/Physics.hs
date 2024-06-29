{-# LANGUAGE RecordWildCards #-}

module Physics
  ( withinRadius
  , updateParticle
  , updatePosition
  , updateVelocity
  , acceleration
  , gravitationalForce
  , distance
  , screenToWorld
  , worldToScreen
  , plus
  , minus
  , gravityConstant
  ) where

import Types
import Data.Bifunctor

gravityConstant :: Float
gravityConstant = 6.67430e-9

timeStep :: Float
timeStep = 0.1

withinRadius :: Position -> Position -> Float -> Bool
withinRadius (x1, y1) (x2, y2) r = distance (x1, y1) (x2, y2) <= r

updateParticle :: [Particle] -> Particle -> Particle
updateParticle particles p@Particle{..} =
  if any isNaN [x, y, vx, vy, ax, ay]
  then p
  else p
    { position = updatePosition position velocity
    , velocity = updateVelocity velocity (acceleration particles p)
    }
  where
    (x, y) = position
    (vx, vy) = velocity
    (ax, ay) = acceleration particles p

updatePosition :: Position -> Velocity -> Position
updatePosition (x, y) (vx, vy) = (x + vx * timeStep, y + vy * timeStep)

updateVelocity :: Velocity -> (Float, Float) -> Velocity
updateVelocity (vx, vy) (ax, ay) = (vx + ax * timeStep, vy + ay * timeStep)

acceleration :: [Particle] -> Particle -> (Float, Float)
acceleration particles p = (sum ax, sum ay)
  where
    (ax, ay) = unzip [gravitationalForce p p' | p' <- particles, p' /= p]

gravitationalForce :: Particle -> Particle -> (Float, Float)
gravitationalForce Particle{..} Particle{position = (x', y'), mass = m'} =
  if dist == 0
  then (0, 0)
  else (fx / mass, fy / mass)
  where
    (x, y) = position
    dx = x' - x
    dy = y' - y
    dist = sqrt (dx * dx + dy * dy)
    force = gravityConstant * mass * m' / (dist * dist)
    fx = force * dx / dist
    fy = force * dy / dist

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

screenToWorld :: State -> (Float, Float) -> (Float, Float)
screenToWorld State{..} (sx, sy) = bimap
  ((sx / viewScale) -) ((sy / viewScale) -) viewTranslate

worldToScreen :: State -> (Float, Float) -> (Float, Float)
worldToScreen State{..} (wx, wy) = bimap
  ((wx * viewScale) +) ((wy * viewScale) +) viewTranslate

plus :: (Float, Float) -> (Float, Float) -> (Float, Float)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minus :: (Float, Float) -> (Float, Float) -> (Float, Float)
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
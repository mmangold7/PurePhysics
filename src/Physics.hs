{-# LANGUAGE RecordWildCards #-}

module Physics 
  ( withinRadius
  , updateParticle
  , acceleration
  , gravitationalForce
  , distance
  , screenToWorld
  , worldToScreen
  , plus
  , minus
  ) where

import Particle
import Types


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
screenToWorld State{..} (sx, sy) = ((sx / viewScale - fst viewTranslate), (sy / viewScale - snd viewTranslate))

worldToScreen :: State -> (Float, Float) -> (Float, Float)
worldToScreen State{..} (wx, wy) = (wx * viewScale + fst viewTranslate, wy * viewScale + snd viewTranslate)

plus :: (Float, Float) -> (Float, Float) -> (Float, Float)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minus :: (Float, Float) -> (Float, Float) -> (Float, Float)
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
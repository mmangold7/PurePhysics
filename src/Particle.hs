{-# LANGUAGE RecordWildCards #-}

module Particle where

import Graphics.Gloss

-- Data types for the particles
data Particle = Particle
  { position :: (Float, Float)
  , velocity :: (Float, Float)
  , mass     :: Float
  } deriving (Eq, Show)

-- Simulation constants
gravityConstant :: Float
gravityConstant = 6.67430e-11

-- Function to draw a particle
drawParticle :: Particle -> Picture
drawParticle Particle{..} = Translate x y (Color white (circleSolid (radius mass)))
  where
    (x, y) = position

-- Function to calculate the radius of a particle based on its mass
radius :: Float -> Float
radius mass = 5 + logBase 2 (mass / 1e6)

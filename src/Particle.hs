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
gravityConstant = 6.67430e-9  -- Increase the gravitational constant for stronger attraction

-- Function to draw a particle
drawParticle :: Particle -> Picture
drawParticle Particle{..} = Translate x y (Color (particleColor mass) (circleSolid (radius mass)))
  where
    (x, y) = position

-- Function to calculate the radius of a particle based on its mass
radius :: Float -> Float
radius mass = 5 + logBase 2 (mass / 1e6)

-- Function to determine the color of a particle based on its mass
particleColor :: Float -> Color
particleColor mass = makeColor r g b 1.0
  where
    ratio = min 1 (logBase 10 mass / 10)
    r = ratio
    g = 1 - ratio
    b = (ratio * (1 - ratio)) ** 0.5

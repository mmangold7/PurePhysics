{-# LANGUAGE RecordWildCards #-}

module Particle 
  ( Particle(..)
  , gravityConstant
  , drawParticle
  , radius
  , particleColor
  ) where

import Graphics.Gloss

data Particle = Particle
  { position :: (Float, Float)
  , velocity :: (Float, Float)
  , mass     :: Float
  } deriving (Eq, Show)

gravityConstant :: Float
gravityConstant = 6.67430e-9

drawParticle :: Particle -> Picture
drawParticle Particle{..} = Translate x y (Color (particleColor mass) (circleSolid (radius mass)))
  where
    (x, y) = position

radius :: Float -> Float
radius mass = 5 + logBase 2 (mass / 1e6)

particleColor :: Float -> Color
particleColor mass = makeColor r g b 1.0
  where
    ratio = min 1 (logBase 10 mass / 10)
    r = ratio
    g = 1 - ratio
    b = (ratio * (1 - ratio)) ** 0.5

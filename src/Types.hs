{-# LANGUAGE RecordWildCards #-}

module Types 
  ( State(..)
  , timeStep
  ) where

import Particle

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

timeStep :: Float
timeStep = 0.1

module Types 
  ( State(..),
    Particle(..)
  ) where

data State = State
  { particles    :: [Particle]
  , dragging     :: Bool
  , dragStart    :: (Float, Float)
  , dragCurrent  :: (Float, Float)
  , dragMass     :: Float
  , dragRadius   :: Float
  , viewScale    :: Float
  , viewTranslate :: (Float, Float)
  , panning      :: Bool
  , panStart     :: (Float, Float)
  , viewStart    :: (Float, Float)
  } deriving (Eq, Show)

data Particle = Particle
  { position :: (Float, Float)
  , velocity :: (Float, Float)
  , mass     :: Float
  } deriving (Eq, Show)
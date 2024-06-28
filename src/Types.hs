module Types 
  ( State(..),
    Particle(..),
    Position,
    Velocity
  ) where

type Position = (Float, Float)
type Velocity = (Float, Float)

data State = State
  { particles    :: [Particle]
  , dragging     :: Bool
  , dragStart    :: Position
  , dragCurrent  :: Position
  , dragMass     :: Float
  , dragRadius   :: Float
  , viewScale    :: Float
  , viewTranslate :: Position
  , panning      :: Bool
  , panStart     :: Position
  , viewStart    :: Position
  } deriving (Eq, Show)

data Particle = Particle
  { position :: Position
  , velocity :: Velocity
  , mass     :: Float
  } deriving (Eq, Show)
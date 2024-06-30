{-# LANGUAGE RecordWildCards #-}

module Types 
  ( State(..),
    Particle(..),
    Position,
    Velocity,
    DrawingMode(..),
    adjustedButtonPos,
    adjustedDrawModeButtonPos,
    adjustedSliderPos
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
  , showDebug    :: Bool
  , drawMode     :: DrawingMode
  , buttonPos    :: Position
  , buttonSize   :: (Float, Float)
  , drawModeButtonPos :: Position
  , drawModeButtonSize :: (Float, Float)
  , timeStep     :: Float
  , sliderPos    :: Position
  , sliderSize   :: (Float, Float)
  , sliderValue  :: Float
  , isSliderActive :: Bool
  , windowSize   :: (Int, Int)
  } deriving (Eq, Show)

data DrawingMode = Filled | Outline | Crosshair deriving (Eq, Show)

data Particle = Particle
  { position :: Position
  , velocity :: Velocity
  , mass     :: Float
  } deriving (Eq, Show)

adjustedButtonPos :: State -> (Float, Float)
adjustedButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 60)

adjustedDrawModeButtonPos :: State -> (Float, Float)
adjustedDrawModeButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 110)

adjustedSliderPos :: State -> (Float, Float)
adjustedSliderPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, - (fromIntegral winHeight / 2) + 40)

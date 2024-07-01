{-# LANGUAGE RecordWildCards #-}

module Types 
  ( State(..),
    Particle(..),
    Position,
    Velocity,
    DrawingMode(..),
    ColorMode(..),
    ArrowSizeMode(..),
    StartingStateMode(..),
    adjustedButtonPos,
    adjustedDrawModeButtonPos,
    adjustedSliderPos,
    adjustedColorModeButtonPos,
    adjustedArrowSizeButtonPos,
    adjustedStartingStateButtonPos
  ) where

import Graphics.Gloss (Color)

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
  , colorMode    :: ColorMode
  , arrowSizeMode :: ArrowSizeMode
  , startingStateMode :: StartingStateMode
  , buttonPos    :: Position
  , buttonSize   :: (Float, Float)
  , drawModeButtonPos :: Position
  , drawModeButtonSize :: (Float, Float)
  , colorModeButtonPos :: Position
  , colorModeButtonSize :: (Float, Float)
  , arrowSizeButtonPos :: Position
  , arrowSizeButtonSize :: (Float, Float)
  , stateButtonPos :: Position
  , stateButtonSize :: (Float, Float)
  , timeStep     :: Float
  , sliderPos    :: Position
  , sliderSize   :: (Float, Float)
  , sliderValue  :: Float
  , isSliderActive :: Bool
  , windowSize   :: (Int, Int)
  } deriving (Eq, Show)

data DrawingMode = Filled | Outline | Crosshair deriving (Eq, Show)

data ColorMode = MassBased | Random deriving (Eq, Show)

data ArrowSizeMode = Constant | Scaled deriving (Eq, Show)

data StartingStateMode = SingleSystem | MultipleSystems | Blank deriving (Eq, Show)

data Particle = Particle
  { position :: Position
  , velocity :: Velocity
  , mass     :: Float
  , color    :: Color
  } deriving (Eq, Show)

adjustedButtonPos :: State -> (Float, Float)
adjustedButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 60)

adjustedDrawModeButtonPos :: State -> (Float, Float)
adjustedDrawModeButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 110)

adjustedColorModeButtonPos :: State -> (Float, Float)
adjustedColorModeButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 160)

adjustedArrowSizeButtonPos :: State -> (Float, Float)
adjustedArrowSizeButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 210)

adjustedStartingStateButtonPos :: State -> (Float, Float)
adjustedStartingStateButtonPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, fromIntegral winHeight / 2 - 260)

adjustedSliderPos :: State -> (Float, Float)
adjustedSliderPos State{..} =
  let (winWidth, winHeight) = windowSize
  in (- (fromIntegral winWidth / 2) + 20, - (fromIntegral winHeight / 2) + 40)

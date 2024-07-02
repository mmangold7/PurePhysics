{-# LANGUAGE RecordWildCards #-}

module Draw 
  ( drawState
  , showParticleInfo
  , determineParticleRadius
  , determineParticleColor
  , randomColors
  ) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss
import Data.Bifunctor
import Types
import Physics (acceleration)
import System.Random
import qualified Data.Text as T

drawState :: State -> Picture
drawState state@State{} = Pictures [drawView state, drawHUD state]

drawView :: State -> Picture
drawView state@State{..} = Scale viewScale viewScale 
           $ uncurry Translate viewTranslate 
           $ Pictures (zipWith (drawParticleWithMode drawMode colorMode arrowSizeMode viewScale showDebug particles) particles [0..] ++ drawDragPicture state)

drawHUD :: State -> Picture
drawHUD state@State{} =
  Pictures
  [ Translate (-390) 290 
    $ Scale 0.1 0.1 
    $ Color white 
    $ Text "HUD Information"
  , drawButton state (adjustedButtonPos state)
  , drawModeButton state (adjustedDrawModeButtonPos state)
  , drawColorModeButton state (adjustedColorModeButtonPos state)
  , drawArrowSizeButton state (adjustedArrowSizeButtonPos state)
  , drawStartingStateButton state (adjustedStartingStateButtonPos state)
  , drawSlider state (adjustedSliderPos state)
  ]

drawButton :: State -> (Float, Float) -> Picture
drawButton State{..} (bx, by) =
  let (bw, bh) = buttonSize
      buttonColor = if showDebug then green else red
      buttonText = if showDebug then "Debug ON" else "Debug OFF"
  in Translate bx by $ Pictures 
     [ Color buttonColor $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text buttonText
     ]

drawModeButton :: State -> (Float, Float) -> Picture
drawModeButton State{..} (bx, by) =
  let (bw, bh) = drawModeButtonSize
      modeText = case drawMode of
                   Filled -> "Mode: Filled"
                   Outline -> "Mode: Outline"
                   Crosshair -> "Mode: Crosshair"
      modeColor = case drawMode of
                    Filled -> red
                    Outline -> green
                    Crosshair -> blue
  in Translate bx by $ Pictures 
     [ Color modeColor $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text modeText
     ]

drawColorModeButton :: State -> (Float, Float) -> Picture
drawColorModeButton State{..} (bx, by) =
  let (bw, bh) = colorModeButtonSize
      colorModeText = case colorMode of
                        MassBased -> "Color: Mass-Based"
                        Random -> "Color: Random"
      colorModeColor = case colorMode of
                         MassBased -> yellow
                         Random -> cyan
  in Translate bx by $ Pictures 
     [ Color colorModeColor $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text colorModeText
     ]

drawArrowSizeButton :: State -> (Float, Float) -> Picture
drawArrowSizeButton State{..} (bx, by) =
  let (bw, bh) = arrowSizeButtonSize
      arrowSizeText = case arrowSizeMode of
                        Constant -> "Arrows: Constant"
                        Scaled -> "Arrows: Scaled"
      arrowSizeColor = case arrowSizeMode of
                         Constant -> magenta
                         Scaled -> orange
  in Translate bx by $ Pictures 
     [ Color arrowSizeColor $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text arrowSizeText
     ]

drawStartingStateButton :: State -> (Float, Float) -> Picture
drawStartingStateButton State{..} (bx, by) =
  let (bw, bh) = stateButtonSize
      stateText = case startingStateMode of
                    SingleSystem -> "State: Single System"
                    MultipleSystems -> "State: Multiple Systems"
                    Types.Blank -> "State: Blank"
      stateColor = case startingStateMode of
                     SingleSystem -> magenta
                     MultipleSystems -> violet
                     Types.Blank -> greyN 0.5
  in Translate bx by $ Pictures 
     [ Color stateColor $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text stateText
     ]

drawSlider :: State -> (Float, Float) -> Picture
drawSlider State{..} (sx, sy) =
  let (sw, sh) = sliderSize
      halfWidth = sw / 2
      sliderValuePos = sx + sliderValue * sw
      originPos = sx + halfWidth
  in Pictures
     [ Translate sx sy $ Color white $ Polygon [(0, 0), (sw, 0), (sw, sh), (0, sh)]
     , Translate originPos sy $ Color blue $ Polygon [(0, 0), (2, 0), (2, sh), (0, sh)] -- Origin line
     , Translate sliderValuePos sy $ Color red $ Polygon [(0, 0), (10, 0), (10, sh), (0, sh)]
     ]

textWidth :: String -> Float
textWidth txt = fromIntegral (T.length (T.pack txt)) * charWidth
  where
    charWidth = 100.0

drawParticleWithMode :: DrawingMode -> ColorMode -> ArrowSizeMode -> Float -> Bool -> [Particle] -> Particle -> Int -> Picture
drawParticleWithMode mode colorMode arrowSizeMode inputScale showDebug allParticles particle index =
  let (x, y) = position particle
      radius = determineParticleRadius (mass particle) * inputScale
      inputColor = case colorMode of
                MassBased -> determineParticleColor (mass particle)
                Random -> inputColor
      basePicture = case mode of
        Filled -> Color inputColor (circleSolid radius)
        Outline -> Color inputColor (circle radius)
        Crosshair -> Pictures 
          [ Color inputColor $ Line [(-radius, 0), (radius, 0)]
          , Color inputColor $ Line [(0, -radius), (0, radius)]
          ]
      len = sqrt ((fst (velocity particle) - x)^2 + (snd (velocity particle) - y)^2)
      scaleInput = case arrowSizeMode of
                Constant -> 1.0
                Scaled -> inputScale
      velocityArrow = if len > 2 then drawArrow (x, y) (bimap (x +) (y +) (bimap (* scaleInput) (* scaleInput) (velocity particle))) green else Graphics.Gloss.Blank
      acc = acceleration allParticles particle
      accelerationArrow = drawArrow (x, y) (bimap (x +) (y +) acc) red
      infoText = showParticleInfo (particle, index)
      textW = textWidth infoText * 0.05 / inputScale
      debugInfo = Translate (x - textW / 2) (y + radius + 10) $ Scale (0.1 / inputScale) (0.1 / inputScale) $ Color white $ Text infoText
  in Pictures [Translate x y basePicture, velocityArrow, accelerationArrow, if showDebug then debugInfo else Graphics.Gloss.Blank]

drawArrow :: (Float, Float) -> (Float, Float) -> Color -> Picture
drawArrow (x1, y1) (x2, y2) inputColor = Color inputColor $ Pictures
  [ Line [(x1, y1), (x2, y2)]
  , Translate x2 y2 $ Rotate (angle x1 y1 x2 y2) $ Polygon [(0, 0), (-10, 5), (-10, -5)]
  ]
  where
    angle inputx1 inputy1 inputx2 inputy2 = 180 * atan2 (inputy1 - inputy2) (inputx2 - inputx1) / pi

showParticleInfo :: (Particle, Int) -> String
showParticleInfo (Particle{..}, index) = 
  show index ++ " V:(" ++ show (round vx :: Integer) ++ ", " ++ show (round vy :: Integer) ++ "), M:" ++ show (round mass :: Integer)
  where
    (x, y) = position
    (vx, vy) = velocity

drawDragPicture :: State -> [Picture]
drawDragPicture State{..} 
  | dragging = [drawArrow dragStart dragCurrent red, drawDragMass dragStart dragMass]
  | otherwise = []

drawDragMass :: (Float, Float) -> Float -> Picture
drawDragMass (x, y) mass = Translate x y (Color (determineParticleColor mass) (circleSolid (determineParticleRadius mass)))

determineParticleColor :: Float -> Color
determineParticleColor mass = makeColor r g b 1.0
  where
    ratio = min 1 (logBase 10 mass / 10)
    r = ratio
    g = 1 - ratio
    b = sqrt (ratio * (1 - ratio))

determineParticleRadius :: Float -> Float
determineParticleRadius mass = 5 + logBase 2 (mass / 1e6)

randomColors :: Int -> IO [Color]
randomColors n = do
  gen <- newStdGen
  let randomList = take (3 * n) (randomRs (0.0, 1.0) gen)
      (rs, gsbs) = splitAt n randomList
      (gs, bs) = splitAt n gsbs
  return $ zipWith3 (\r g b -> makeColor r g b 1.0) rs gs bs

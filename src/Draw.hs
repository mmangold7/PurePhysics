{-# LANGUAGE RecordWildCards #-}

module Draw 
  ( drawState, 
    showParticleInfo, 
    determineParticleRadius 
  ) where

import Graphics.Gloss
import Types
import Physics (acceleration)

drawState :: State -> Picture
drawState state@State{} = Pictures [drawView state, drawHUD state]

drawView :: State -> Picture
drawView state@State{..} = Scale viewScale viewScale 
           $ uncurry Translate viewTranslate 
           $ Pictures (map (drawParticleWithMode drawMode viewScale showDebug particles) particles ++ drawDragPicture state)

drawHUD :: State -> Picture
drawHUD state@State{..} =
  Pictures
  [ Translate (-390) 290 
    $ Scale 0.1 0.1 
    $ Color white 
    $ Text "HUD Information"
  , drawButton state (adjustedButtonPos state)
  , drawModeButton state (adjustedDrawModeButtonPos state)
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
  in Translate bx by $ Pictures 
     [ Color blue $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text modeText
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

drawParticleWithMode :: DrawingMode -> Float -> Bool -> [Particle] -> Particle -> Picture
drawParticleWithMode mode inputScale showDebug allParticles particle@Particle{..} =
  let (x, y) = position
      radius = determineParticleRadius mass * inputScale
      basePicture = case mode of
        Filled -> Color (determineParticleColor mass) (circleSolid radius)
        Outline -> Color (determineParticleColor mass) (circle radius)
        Crosshair -> Pictures 
          [ Color (determineParticleColor mass) $ Line [(-radius, 0), (radius, 0)]
          , Color (determineParticleColor mass) $ Line [(0, -radius), (0, radius)]
          ]
      velocityArrow = drawArrow (x, y) ((x + fst velocity), (y + snd velocity)) green
      acc = acceleration allParticles particle
      accelerationArrow = drawArrow (x, y) ((x + fst acc), (y + snd acc)) red
      debugInfo = Translate (x + radius + 10) (y + radius + 10) $ Scale 0.1 0.1 $ Color white $ Text (showParticleInfo (particle, 0))
  in Pictures [Translate x y basePicture, velocityArrow, accelerationArrow, if showDebug then debugInfo else Blank]

drawArrow :: (Float, Float) -> (Float, Float) -> Color -> Picture
drawArrow (x1, y1) (x2, y2) color = Color color $ Pictures
  [ Line [(x1, y1), (x2, y2)]
  , Translate x2 y2 $ Rotate (angle x1 y1 x2 y2) $ Polygon [(0, 0), (-10, 5), (-10, -5)]
  ]
  where
    angle inputx1 inputy1 inputx2 inputy2 = 180 * atan2 (inputy1 - inputy2) (inputx2 - inputx1) / pi

showParticleInfo :: (Particle, Int) -> String
showParticleInfo (Particle{..}, index) = 
  "Particle " ++ show index ++ ": Pos=(" ++ show (round x :: Integer) ++ "," ++ show (round y :: Integer) 
  ++ "), Vel=(" ++ show (round vx :: Integer) ++ "," ++ show (round vy :: Integer) ++ "), Mass=" ++ show (round mass :: Integer)
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

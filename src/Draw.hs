{-# LANGUAGE RecordWildCards #-}

module Draw 
  ( drawState, 
    showParticleInfo, 
    determineParticleRadius 
  ) where

import Graphics.Gloss
import Types

drawState :: State -> Picture
drawState state@State{} = Pictures [drawView state, drawHUD state]

drawView :: State -> Picture
drawView state@State{..} = Scale viewScale viewScale 
           $ uncurry Translate viewTranslate 
           $ Pictures (concatMap (drawParticleWithInfo showDebug) particles ++ drawDragPicture state)

drawHUD :: State -> Picture
drawHUD state@State{} = Pictures
  [ Translate (-390) 290 
    $ Scale 0.1 0.1 
    $ Color white 
    $ Text "HUD Information" -- Modify this to show only general HUD information
  , drawButton state
  ]

drawButton :: State -> Picture
drawButton State{..} =
  let (bx, by) = buttonPos
      (bw, bh) = buttonSize
      buttonColor = if showDebug then green else red
      buttonText = if showDebug then "Debug ON" else "Debug OFF"
  in Translate bx by $ Pictures 
     [ Color buttonColor $ Polygon [(0, 0), (bw, 0), (bw, bh), (0, bh)]
     , Translate 10 10 $ Scale 0.1 0.1 $ Color white $ Text buttonText
     ]

drawParticleWithInfo :: Bool -> Particle -> [Picture]
drawParticleWithInfo showDebug particle@Particle{..} =
  let basePicture = Translate x y (Color (determineParticleColor mass) (circleSolid (determineParticleRadius mass)))
  in if showDebug 
     then [basePicture, Translate (x + 10) (y + 10) $ Scale 0.1 0.1 $ Color white $ Text (showParticleInfo particle)]
     else [basePicture]
  where
    (x, y) = position

showParticleInfo :: Particle -> String
showParticleInfo Particle{..} = 
  "Pos=(" ++ show (round x :: Integer) ++ "," ++ show (round y :: Integer) 
  ++ "), Vel=(" ++ show (round vx :: Integer) ++ "," ++ show (round vy :: Integer) ++ "), Mass=" ++ show (round mass :: Integer)
  where
    (x, y) = position
    (vx, vy) = velocity

drawDragPicture :: State -> [Picture]
drawDragPicture State{..} 
  | dragging = [drawArrow dragStart dragCurrent, drawDragMass dragStart dragMass]
  | otherwise = []

drawArrow :: (Float, Float) -> (Float, Float) -> Picture
drawArrow (x1, y1) (x2, y2) = Color red $ Pictures
  [ Line [(x1, y1), (x2, y2)]
  , Translate x2 y2 $ Rotate (angle y1 x2 y2) $ Polygon [(0, 0), (-10, 5), (-10, -5)]
  ]
  where
    angle angley1 anglex2 angley2 = 180 * atan2 (angley1 - angley2) (anglex2 - angley1) / pi

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

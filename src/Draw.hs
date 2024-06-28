{-# LANGUAGE RecordWildCards #-}

module Draw 
  ( drawState
  , drawParticles
  , drawArrow
  , drawDragMass
  , showParticleInfo
  ) where

import Graphics.Gloss
import Particle
import Types

drawState :: State -> Picture
drawState state@State{..} = Pictures [view, hud]
  where
    view = Scale viewScale viewScale (Translate (fst viewTranslate) (snd viewTranslate) (Pictures (drawParticles particles ++ dragPicture)))
    hud = Translate (-390) 290 $ Scale 0.1 0.1 $ Color white $ Text (unlines (map showParticleInfo (zip particles [0..])))
    dragPicture = if dragging
                  then [drawArrow dragStart dragCurrent, drawDragMass dragStart dragMass]
                  else []

drawParticles :: [Particle] -> [Picture]
drawParticles particles = map drawParticle particles

drawArrow :: (Float, Float) -> (Float, Float) -> Picture
drawArrow (x1, y1) (x2, y2) = Color red $ Pictures
  [ Line [(x1, y1), (x2, y2)]
  , Translate x2 y2 $ Rotate (angle x1 y1 x2 y2) $ Polygon [(0, 0), (-10, 5), (-10, -5)]
  ]
  where
    angle x1 y1 x2 y2 = 180 * atan2 (y1 - y2) (x2 - x1) / pi

drawDragMass :: (Float, Float) -> Float -> Picture
drawDragMass (x, y) mass = Translate x y (Color (particleColor mass) (circleSolid (radius mass)))

showParticleInfo :: (Particle, Int) -> String
showParticleInfo (Particle{..}, index) = "Particle " ++ show index ++ ": Pos=(" ++ show (round x) ++ "," ++ show (round y) ++ "), Vel=(" ++ show (round vx) ++ "," ++ show (round vy) ++ "), Mass=" ++ show (round mass)
  where
    (x, y) = position
    (vx, vy) = velocity

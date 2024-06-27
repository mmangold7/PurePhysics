{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Particle
import Simulation

-- Main function
main :: IO ()
main = play
  (InWindow "2D Gravity Simulation" (800, 600) (100, 100))
  black
  60
  initialState
  drawState
  handleInput
  updateState

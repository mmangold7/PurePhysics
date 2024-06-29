module SimulationSpec (spec) where

import Test.Hspec
import Simulation
import Types
import Graphics.Gloss.Interface.Pure.Game

spec :: Spec
spec = do
  describe "Simulation.initialState" $ do
    it "initializes state correctly" $ do
      let state = initialState
      viewScale state `shouldBe` 1
      particles state `shouldBe` initialParticles

  describe "Simulation.handleInput" $ do
    it "handles mouse down correctly" $ do
      let state = initialState
      let newState = handleMouseDown 100 100 state
      dragging newState `shouldBe` True
      dragStart newState `shouldBe` (100, 100)

    it "handles mouse motion correctly" $ do
      let state = handleMouseDown 100 100 initialState
      let newState = handleMouseMotion 200 200 state
      dragCurrent newState `shouldBe` (200, 200)

    it "handles mouse up correctly" $ do
      let state = handleMouseDown 100 100 initialState
      let newState = handleMouseUp 200 200 state
      dragging newState `shouldBe` False
      length (particles newState) `shouldBe` 3 -- One new particle added

  describe "Simulation.updateState" $ do
    it "updates state correctly" $ do
      let state = initialState
      let newState = updateState 0.1 state
      length (particles newState) `shouldBe` 2

    it "updates particle positions and velocities" $ do
      let state = initialState
      let p1 = head (particles state)
      let newState = updateState 0.1 state
      let newP1 = head (particles newState)
      position newP1 `shouldNotBe` position p1
      velocity newP1 `shouldNotBe` velocity p1

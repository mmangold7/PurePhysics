module PhysicsSpec (spec) where

import Test.Hspec
import Physics
import Types

tolerance :: Float
tolerance = 1e-9

approxEqual :: (Float, Float) -> (Float, Float) -> Bool
approxEqual (x1, y1) (x2, y2) = abs (x1 - x2) < tolerance && abs (y1 - y2) < tolerance

spec :: Spec
spec = do
  describe "Physics.distance" $ do
    it "calculates the distance between two points correctly" $ do
      distance (0, 0) (3, 4) `shouldBe` 5.0

    it "returns 0 for identical points" $ do
      distance (1, 1) (1, 1) `shouldBe` 0.0

    it "handles negative coordinates" $ do
      distance (-1, -1) (1, 1) `shouldBe` sqrt 8.0

  describe "Physics.plus" $ do
    it "adds two vectors correctly" $ do
      plus (1, 2) (3, 4) `shouldBe` (4, 6)

  describe "Physics.minus" $ do
    it "subtracts two vectors correctly" $ do
      minus (5, 6) (2, 3) `shouldBe` (3, 3)

  describe "Physics.gravitationalForce" $ do
    it "calculates gravitational force correctly" $ do
      let p1 = Particle (0, 0) (0, 0) 5.0
      let p2 = Particle (3, 4) (0, 0) 10.0
      let expectedForce = 
            let dx = 3.0
                dy = 4.0
                dist = sqrt (dx * dx + dy * dy)
                force = Physics.gravityConstant * 5.0 * 10.0 / (dist * dist)
                fx = force * dx / dist
                fy = force * dy / dist
            in (fx, fy)
      gravitationalForce p1 p2 `shouldSatisfy` approxEqual expectedForce

    it "returns zero force for overlapping particles" $ do
      let p1 = Particle (0, 0) (0, 0) 5.0
      gravitationalForce p1 p1 `shouldBe` (0.0, 0.0)

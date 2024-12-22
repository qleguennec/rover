module Solver.Spec where

import Data.Maybe
import Linear.V2
import Solver
import Test.Hspec
import Types
import Types (RobotSpec (RobotSpec), World (_robotSpecs))

spec :: Spec
spec = do
  describe "rotate orientation" $ do
    -- TODO: Use quickcheck
    it "Should return after 4 rotations" $ do
      (rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation) North `shouldBe` North
      (rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation) East `shouldBe` East
      (rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation) South `shouldBe` South
      (rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation . rotateOrientation LeftRotation) West `shouldBe` West
      (rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation) North `shouldBe` North
      (rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation) East `shouldBe` East
      (rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation) South `shouldBe` South
      (rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation . rotateOrientation RightRotation) West `shouldBe` West

  describe "solver primitives" $ do
    it "Should return the same robot spec when the robot is already lost" $ do
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 20 20, _orientation = North}, _commands = [CommandFW, CommandFW]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 20 20, _orientation = North}, _isLost = True})]

    it "Should be lost" $ do
      runWorld (World {_dimensions = (0, 0), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 20 20, _orientation = North}, _commands = [CommandFW, CommandFW]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 20 20, _orientation = North}, _isLost = True})]
      isLost (0, 0) (V2 0 1) `shouldBe` True
      isLost (0, 0) (V2 0 (-1)) `shouldBe` True
      runRobotSpec (0, 0) RobotSpec {_spatialState = SpatialState {_position = V2 1 1, _orientation = South}, _commands = []}
        `shouldBe` Nothing
      runWorld (World {_dimensions = (0, 0), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 0 0, _orientation = South}, _commands = [CommandFW]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 0 0, _orientation = South}, _isLost = True})]

    it "Should not be lost on edge cases" $ do
      runWorld (World {_dimensions = (0, 0), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 0 0, _orientation = North}, _commands = []}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 0 0, _orientation = North}, _isLost = False})]

  describe "solver" $ do
    it "Should return to the initial position" $ do
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = North}, _commands = [CommandFW, CommandR LeftRotation, CommandR LeftRotation, CommandFW]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = South}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = North}, _commands = [CommandFW, CommandR RightRotation, CommandR RightRotation, CommandFW]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = South}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = North}, _commands = [CommandFW, CommandR LeftRotation, CommandR LeftRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = North}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = East}, _commands = [CommandFW, CommandR LeftRotation, CommandR LeftRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = East}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = East}, _commands = [CommandFW, CommandR RightRotation, CommandR RightRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = East}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = South}, _commands = [CommandFW, CommandR LeftRotation, CommandR LeftRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = South}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = South}, _commands = [CommandFW, CommandR RightRotation, CommandR RightRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = South}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = West}, _commands = [CommandFW, CommandR LeftRotation, CommandR LeftRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = West}, _isLost = False})]
      runWorld (World {_dimensions = (10, 10), _robotSpecs = [RobotSpec {_spatialState = SpatialState {_position = V2 5 5, _orientation = West}, _commands = [CommandFW, CommandR RightRotation, CommandR RightRotation, CommandFW, CommandR RightRotation, CommandR RightRotation]}]})
        `shouldBe` [(Result {_state = SpatialState {_position = V2 5 5, _orientation = West}, _isLost = False})]

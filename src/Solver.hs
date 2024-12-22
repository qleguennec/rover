module Solver (runWorld, showResult, rotateOrientation, isLost, runRobotSpec) where

import Control.Lens
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Linear.V2
import Types

orientationToV2 :: Orientation -> V2 Int
orientationToV2 North = V2 0 1
orientationToV2 East = V2 1 0
orientationToV2 South = V2 0 (-1)
orientationToV2 West = V2 (-1) 0

rotateOrientation :: Rotation -> Orientation -> Orientation
rotateOrientation LeftRotation North = West
rotateOrientation RightRotation North = East
rotateOrientation LeftRotation East = North
rotateOrientation RightRotation East = South
rotateOrientation LeftRotation South = East
rotateOrientation RightRotation South = West
rotateOrientation LeftRotation West = South
rotateOrientation RightRotation West = North

executeCommand :: SpatialState -> Command -> SpatialState
executeCommand state@(SpatialState {_position, _orientation}) CommandFW = set position (_position + orientationToV2 _orientation) state
executeCommand state (CommandR rotation) = over orientation (rotateOrientation rotation) state

-- | Computes whether the robot is lost or not. The robot is considered lost if either of its position coordinates is outside the map.
isLost :: Dimensions -> Position -> IsLost
isLost (w, h) (V2 x y) = x `notElem` [0 .. w] || y `notElem` [0 .. h]

-- | Runs all the robot commands in the queue. Returns the last valid (non-lost) spatial state of the robot, along with the IsLost boolean
runRobotSpec :: Dimensions -> RobotSpec -> Maybe (N.NonEmpty SpatialState)
runRobotSpec dim (RobotSpec {_spatialState, _commands}) = N.nonEmpty . N.takeWhile (not . isLost dim . view position) . N.scanl executeCommand _spatialState $ _commands

computeResult :: RobotSpec -> Maybe (N.NonEmpty SpatialState) -> Result
computeResult (RobotSpec {_spatialState, _commands}) = maybe (Result {_state = _spatialState, _isLost = True}) (\states -> Result {_state = N.last states, _isLost = length states - 1 < length _commands})

runWorld :: World -> [Result]
runWorld (World {_dimensions, _robotSpecs}) = map (\spec -> computeResult spec $ runRobotSpec _dimensions spec) _robotSpecs

showResult :: Result -> String
showResult (Result {_state, _isLost}) =
  let (V2 x y) = view position _state
   in "("
        <> intercalate ", " [show x, show y, take 1 . show . view orientation $ _state]
        <> ")"
        <> (if _isLost then " LOST" else "")

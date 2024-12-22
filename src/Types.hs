{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Command (..),
    Dimensions,
    Orientation (..),
    RobotSpec (..),
    Rotation (..),
    SpatialState (..),
    World (..),
    spatialState,
    position,
    orientation,
    robotSpecs,
    IsLost,
    Result (..),
    Position,
  )
where

import Control.Lens
import qualified Data.Either as E
import Linear.V2

type Width = Int

type Height = Int

type Dimensions = (Width, Height)

type Position = V2 Int

type IsLost = Bool

data Orientation = North | East | South | West
  deriving (Show, Eq)

data SpatialState = SpatialState {_position :: Position, _orientation :: Orientation}
  deriving (Show, Eq)

makeLenses ''SpatialState

data Rotation = LeftRotation | RightRotation
  deriving (Show, Eq)

data Command = CommandFW | CommandR Rotation
  deriving (Show, Eq)

data RobotSpec = RobotSpec {_spatialState :: SpatialState, _commands :: [Command]}
  deriving (Show, Eq)

makeLenses ''RobotSpec

data World = World {_dimensions :: Dimensions, _robotSpecs :: [RobotSpec]}
  deriving (Show, Eq)

makeLenses ''World

data Result = Result {_state :: SpatialState, _isLost :: IsLost}
  deriving (Show, Eq)

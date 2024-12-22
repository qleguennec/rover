module Parser (parse) where

import qualified Data.Either as E
import qualified Data.Text as T
import Lib
import Linear.V2
import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Types (Command (..), Dimensions, Orientation (..), RobotSpec (..), Rotation (..), SpatialState (..), World (..))

parseOrientation :: Char -> Either Orientation Char
parseOrientation 'N' = Left North
parseOrientation 'E' = Left East
parseOrientation 'S' = Left South
parseOrientation 'W' = Left West
parseOrientation c = Right c

parseCommand :: Char -> Either Command Char
parseCommand 'F' = Left CommandFW
parseCommand 'L' = Left $ CommandR LeftRotation
parseCommand 'R' = Left $ CommandR RightRotation
parseCommand c = Right c

dimensions :: Parsec T.Text () Dimensions
dimensions = liftA2 (,) (digits' <* spaces') digits'

orientation :: Parsec T.Text () Orientation
orientation = E.fromLeft (error "Unknown orientation") . parseOrientation <$> oneOf "NESW"

command :: Parsec T.Text () Command
command = E.fromLeft (error "Unknown command") . parseCommand <$> oneOf "LFR"

commands :: Parsec T.Text () [Command]
commands = many1 command

robotSpatialState :: Parsec T.Text () SpatialState
robotSpatialState =
  between (char '(') (char ')') $
    liftA2
      SpatialState
      (liftA2 V2 (digits' <* comma <* spaces') (digits' <* comma <* spaces'))
      orientation

robotSpec :: Parsec T.Text () RobotSpec
robotSpec = liftA2 RobotSpec (robotSpatialState <* spaces') commands

worldParser :: Parsec T.Text () World
worldParser = liftA2 World (dimensions <* newline) (robotSpec `sepEndBy1` newline) <* eof

parse :: SourceName -> T.Text -> Either ParseError World
parse = P.parse worldParser

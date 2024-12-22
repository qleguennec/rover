{-# LANGUAGE OverloadedStrings #-}

module Parser.Spec where

import Data.Either
import Data.Text
import qualified Data.Text as T
import Linear.V2
import qualified Parser as P
import Test.Hspec
import Text.Parsec hiding (parse)
import Types
import Types (Rotation (RightRotation))
import Prelude hiding (unlines)

parse :: T.Text -> Either ParseError World
parse = P.parse "tests"

spec :: Spec
spec = do
  describe "parser" $ do
    it "Should fail for empty input" $
      parse "" `shouldSatisfy` isLeft

    it "Should fail with just dimensions" $
      parse "0 2" `shouldSatisfy` isLeft

    it "Should fail with non digits dimensions" $
      parse "ab cd" `shouldSatisfy` isLeft

    it "Should fail for just dimension + spatial info" $
      parse (unlines ["0 1", "(0, 1, E)"]) `shouldSatisfy` isLeft

    it "Should return world for valid input" $ do
      parse (unlines ["0 1", "(0, 1, E) LRF"])
        `shouldBe` Right
          ( World
              { _dimensions = (0, 1),
                _robotSpecs =
                  [ RobotSpec
                      { _spatialState = SpatialState {_position = V2 0 1, _orientation = East},
                        _commands =
                          [ CommandR LeftRotation,
                            CommandR RightRotation,
                            CommandFW
                          ]
                      }
                  ]
              }
          )
      parse (unlines ["0               1", "(0,    1,    E) LRF"])
        `shouldBe` Right
          ( World
              { _dimensions = (0, 1),
                _robotSpecs =
                  [ RobotSpec
                      { _spatialState = SpatialState {_position = V2 0 1, _orientation = East},
                        _commands =
                          [ CommandR LeftRotation,
                            CommandR RightRotation,
                            CommandFW
                          ]
                      }
                  ]
              }
          )
      parse "0 1\n(0, 1, E) LRF"
        `shouldBe` Right
          ( World
              { _dimensions = (0, 1),
                _robotSpecs =
                  [ RobotSpec
                      { _spatialState = SpatialState {_position = V2 0 1, _orientation = East},
                        _commands =
                          [ CommandR LeftRotation,
                            CommandR RightRotation,
                            CommandFW
                          ]
                      }
                  ]
              }
          )
      parse "0 1\n(0, 1, E) LRF\n"
        `shouldBe` Right
          ( World
              { _dimensions = (0, 1),
                _robotSpecs =
                  [ RobotSpec
                      { _spatialState = SpatialState {_position = V2 0 1, _orientation = East},
                        _commands =
                          [ CommandR LeftRotation,
                            CommandR RightRotation,
                            CommandFW
                          ]
                      }
                  ]
              }
          )
      parse (unlines ["0 1", "(0, 1, S) LRF", "(10, 20, N) LRRFRF"])
        `shouldBe` Right
          ( World
              { _dimensions = (0, 1),
                _robotSpecs =
                  [ RobotSpec {_spatialState = SpatialState {_position = V2 0 1, _orientation = South}, _commands = [CommandR LeftRotation, CommandR RightRotation, CommandFW]},
                    RobotSpec
                      { _spatialState = SpatialState {_position = V2 10 20, _orientation = North},
                        _commands =
                          [ CommandR LeftRotation,
                            CommandR RightRotation,
                            CommandR RightRotation,
                            CommandFW,
                            CommandR RightRotation,
                            CommandFW
                          ]
                      }
                  ]
              }
          )

    it "Should fail for more than 2 coordinates" $ do
      parse (unlines ["0 1 2", "(0, 1, E, N) LRF"]) `shouldSatisfy` isLeft

    it "Should fail for more less 2 coordinates" $ do
      parse (unlines ["0 ", "(0, 1, E, N) LRF"]) `shouldSatisfy` isLeft
      parse (unlines ["0", "(0, 1, E, 0) LRF"]) `shouldSatisfy` isLeft

    it "Should fail for more than 3 spatial coordinates" $ do
      parse (unlines ["0 1", "(0, 1, E, N) LRF"]) `shouldSatisfy` isLeft
      parse (unlines ["0 1", "(0, 1, E, 0) LRF"]) `shouldSatisfy` isLeft

    it "Should fail for less than 3 spatial coordinates" $ do
      parse (unlines ["0 1", "(0, 1) LRF"]) `shouldSatisfy` isLeft
      parse (unlines ["0 1", "(0) LRF"]) `shouldSatisfy` isLeft
      parse (unlines ["0 1", "() LRF"]) `shouldSatisfy` isLeft

    it "Should fail for non digits spatial coordinates" $ do
      parse (unlines ["0 1", "(N, E, E) LRF"]) `shouldSatisfy` isLeft

    it "Should fail for non NESW orientations" $ do
      parse (unlines ["0 1", "(0, 1, F) LRF"]) `shouldSatisfy` isLeft

    it "Should fail for non LRF commands" $ do
      parse (unlines ["0 1", "(0, 1, E) T"]) `shouldSatisfy` isLeft
      parse (unlines ["0 1", "(0, 1, E) LRTR"]) `shouldSatisfy` isLeft
      parse (unlines ["0 1", "(0, 1, E) LRF", "(10, 20, N) LRTFRF"]) `shouldSatisfy` isLeft

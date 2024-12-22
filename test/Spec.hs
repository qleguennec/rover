module Spec where

import qualified Main
import qualified Parser.Spec
import qualified Solver.Spec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser" Parser.Spec.spec
  describe "Solver" Solver.Spec.spec
  describe "File Solver" $ do
    it "Should correctly solve the file" $ do
      Main.solveFile "resources/examples/01" `shouldReturn` Right ["(4, 4, E)", "(0, 4, W) LOST"]
      Main.solveFile "resources/examples/02" `shouldReturn` Right ["(2, 3, W)", "(1, 0, S) LOST"]

module Main where

import Control.Monad
import Data.Text.IO as TIO hiding (putStrLn)
import Parser
import Solver
import System.Directory
import System.Environment
import Text.Parsec hiding (parse)
import Types

solveFile :: FilePath -> IO (Either ParseError [String])
solveFile fp = do
  contents <- TIO.readFile fp
  pure $ map showResult . runWorld <$> parse fp contents

printResult :: FilePath -> IO ()
printResult fp = do
  results <- solveFile fp
  case results of
    (Left parseError) -> print parseError
    (Right r) -> forM_ r putStrLn

mainWithArgs :: IO ()
mainWithArgs = withArgs ["resources/examples/01"] main

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "You must specify exactly one file to solve!"
    else printResult (head args)

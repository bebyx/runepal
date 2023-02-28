module Main (main) where

import System.Environment (getArgs)
import System.Random (getStdGen)
import Util (handleArgs)

main :: IO ()
main = do
  args <- getArgs
  gen <- getStdGen
  let output = handleArgs args gen
  putStrLn output

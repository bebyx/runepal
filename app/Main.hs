module Main (main) where

import Rune
import Util
import System.Environment
import System.Random

main :: IO ()
main = do
  args <- getArgs
  seed <- getStdGen
  let rune = handleArgs args seed

  putChar ' '
  putChar $ unicode rune
  putStrLn $ " (" ++ [transliteration rune] ++ ")"
  putStrLn $ (show $ name rune) ++ ": " ++ meaning rune

module Main (main) where

import Rune
import Util
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let rune = handleArgs args

  putChar ' '
  putChar $ unicode rune
  putStrLn $ " (" ++ [transliteration rune] ++ ")"
  putStrLn $ (show $ name rune) ++ ": " ++ meaning rune

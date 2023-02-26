module Main (main) where

import Rune
import Util
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let runeString = if null args then "Fehu" else capitalize $ args !! 0 -- ^ show random rune instead of Fehu placeholder USE GUARDS!!!
      rune = if runeString `elem` map show futhark
             then getDataFor $ read $ runeString
             else error "Not a Futhark rune provided" -- ^ or just print random rune: Not a Futhark rune provided, printing random rune...

  putChar ' '
  putChar $ unicode rune
  putStrLn $ " (" ++ [transliteration rune] ++ ")"
  putStrLn $ (show $ name rune) ++ ": " ++ meaning rune


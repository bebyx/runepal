module Main (main) where

import Lib

main :: IO ()
main = do
  let rune = getDataFor Fehu

  putChar ' '
  putChar $ unicode rune
  putStrLn $ " (" ++ [transliteration rune] ++ ")"
  putStrLn $ (show $ name rune) ++ ": " ++ meaning rune

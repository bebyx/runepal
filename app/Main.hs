module Main (main) where

import Rune (RuneData(..), getDataFor)
import System.Environment (getArgs)
import System.Random (getStdGen)
import Util (handleArgs)

main :: IO ()
main = do
  args <- getArgs
  gen <- getStdGen
  let result = handleArgs args gen

  case result of
    Left string -> putStrLn string
    Right rune ->
      do
        let runeData = getDataFor rune
        putChar ' '
        putChar $ unicode runeData
        putStrLn $ " (" ++ [transliteration runeData] ++ ")"
        putStrLn $ show (name runeData) ++ ": " ++ meaning runeData

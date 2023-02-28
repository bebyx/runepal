{-# LANGUAGE QuasiQuotes #-}

module Util (handleArgs) where

import Data.Char (isLetter, toLower, toUpper)
import Data.List (intercalate)
import Data.String.Here (here)
import Data.String.Here.Interpolated ( i )
import Rune (Rune, RuneData(..), getDataFor)
import System.Random (random, StdGen)
import Text.Read (readMaybe)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

handleArgs :: [String] -> StdGen -> String
handleArgs [] gen = constructOutputFor rune
  where (rune, _) = random gen
handleArgs ("help":_) _ = helpMessage
handleArgs ("list":_) _ = intercalate "\n" $ map show futhark -- ^ not unlines to avoid newline in the very end 
  where futhark = [minBound .. ] :: [Rune]
handleArgs (input:_) gen = handleRuneInput probablyRune
  where
    normalizedInput = capitalize $ filter isLetter input
    probablyRune = readMaybe normalizedInput
    handleRuneInput Nothing = "Not a Futhark rune provided, dropping random rune…\n"
                              ++ handleArgs [] gen
    handleRuneInput (Just rune) = constructOutputFor rune

constructOutputFor :: Rune -> String
constructOutputFor rune = [i|  ${[unicode info]} (${[transliteration info]})
 ${name info}: ${meaning info} |]
  where
    info = getDataFor $ Just rune

helpMessage :: String
helpMessage = [here|
  Usage: command [arg]
  * no arg -- print data for random Futhark rune
  * rune name (case insensitive) -- print data for a rune of your choice
  * list -- list all Futhark runes
  * help -- print this help message
  |]


{-# LANGUAGE QuasiQuotes #-}

module Util (handleArgs) where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.String.Here (here)
import Rune (Rune)
import System.Random (random, StdGen)
import Text.Read (readMaybe)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

handleArgs :: [String] -> StdGen -> Either String (Maybe Rune)
handleArgs [] gen = Right $ Just rune
  where (rune, _) = random gen
handleArgs ("help":_) _ = Left helpMessage
handleArgs ("list":_) _ = Left $ intercalate "\n" $ map show futhark -- ^ not unlines to avoid newline in the very end 
  where futhark = [minBound .. ] :: [Rune]
handleArgs (input:_) _ = Right $ readMaybe normalizedInput
  where normalizedInput = capitalize input

helpMessage :: String
helpMessage = [here|
  Usage: command [arg]
  * no arg -- print data for random Futhark rune
  * rune name (case insensitive) -- print data for a rune of your choice
  * list -- list all Futhark runes
  * help -- print this help message
  |]


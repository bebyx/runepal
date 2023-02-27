module Util (handleArgs) where

import Data.Char (toLower, toUpper)
import Rune

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

handleArgs :: [String] -> RuneData
handleArgs [] = getDataFor Fehu -- ^ show random rune instead of Fehu placeholder
handleArgs (input:_)
  | normalizedInput `elem` map show futhark = getDataFor $ read normalizedInput
  | otherwise = error "Not a Futhark rune provided"
  where
    normalizedInput = capitalize input

-- | Function to get random element of a list also goes here.

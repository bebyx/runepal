module Util (handleArgs) where

import Data.Char (toLower, toUpper)
import Rune
import System.Random

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

handleArgs :: [String] -> StdGen -> RuneData
handleArgs [] gen = getDataFor rune -- ^ show random rune
  where (rune, _) = random gen
handleArgs (input:_) _
  | normalizedInput `elem` map show futhark = getDataFor $ read normalizedInput
  | otherwise = error "Not a Futhark rune provided"
  where normalizedInput = capitalize input

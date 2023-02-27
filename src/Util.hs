module Util (handleArgs) where

import Data.Char (toLower, toUpper)
import Rune
import System.Random
import Text.Read (readMaybe)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

handleArgs :: [String] -> StdGen -> RuneData
handleArgs [] gen = getDataFor $ Just rune
  where (rune, _) = random gen
handleArgs (input:_) _ = getDataFor $ readMaybe $ normalizedInput
  where normalizedInput = capitalize input
      

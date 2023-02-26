module Util where

import Data.Char (toLower, toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

-- | Function to get random element of a list also goes here.

{-# LANGUAGE QuasiQuotes #-}

module Util (handleArgs) where

import Data.Char (isLetter, toLower, toUpper)
import Data.List (intercalate)
import Data.String.Here.Uninterpolated (here)
import Data.String.Here.Interpolated ( i, iTrim )
import Futhark (Rune, RuneData(..), getDataFor)
import System.Random (random, StdGen)
import Text.PrettyPrint.Boxes (left, moveRight, para, render)
import Text.Read (readMaybe)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

handleArgs :: [String] -> StdGen -> String
handleArgs [] gen = constructStandardOutput rune
  where (rune, _) = random gen
handleArgs ("help":_) _ = helpMessage
handleArgs ("list":_) _ = intercalate "\n" $ map show futhark -- ^ not unlines to avoid newline in the very end
  where futhark = [minBound .. ] :: [Rune]
handleArgs ("csv":_) _ = constructCsvOutput
handleArgs (input:_) gen = handleRuneInput probablyRune
  where
    normalizedInput = capitalize $ filter isLetter input
    probablyRune = readMaybe normalizedInput
    handleRuneInput Nothing = "Not a Futhark rune provided, dropping random rune…\n\n"
                              ++ handleArgs [] gen
    handleRuneInput (Just rune) = constructStandardOutput rune

constructStandardOutput :: Rune -> String
constructStandardOutput rune = [i|  ${[unicode info]} (${[transliteration info]})
 ${name info}: ${meaning info}
 Aett: ${aett info}
 Divination:
${prettifiedDivination} |]
  where
    info = getDataFor rune
    prettifiedDivination = render . moveRight 2 . para left 60 $ divination info

constructCsvOutput :: String
constructCsvOutput = foldl (\acc x -> acc ++ (row $ getDataFor x) ++ "\n") "Name,Tranliteration,Unicode,Aett,Meaning,Divination\n" futhark
  where
    row info =
      [iTrim|${name info},${[transliteration info]},${[unicode info]},${aett info},"${meaning info}","${normalize $ divination info}"|]
    futhark = [minBound .. ] :: [Rune]
    normalize = intercalate "" . map escapeQuotes
    escapeQuotes '"' = "\"\""
    escapeQuotes c = [c]

helpMessage :: String
helpMessage = [here|
  Usage: command [arg]
  * no arg -- print data for random Futhark rune
  * rune name (case insensitive) -- print data for a rune of your choice
  * list -- list all Futhark runes
  * help -- print this help message
  |]

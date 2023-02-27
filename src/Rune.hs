module Rune where
import System.Random

data Rune = Fehu
          | Uruz
          | Thurisaz
          | Ansuz
          | Raido
          | Kaunan
          | Gebo
          | Wunjo
          | Hagalaz
          | Naudiz
          | Isaz
          | Jera
          | Eiwaz
          | Perth
          | Algiz
          | Sowilo
          | Tiwaz
          | Berkanan
          | Ehwaz
          | Mannaz
          | Laguz
          | Ingwaz
          | Othala
          | Dagaz deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Random Rune where
  randomR (lo, hi) gen =
    let (randInt, newGen) = randomR (fromEnum lo, fromEnum hi) gen
    in (toEnum randInt :: Rune, newGen)
  random gen = randomR (minBound, maxBound) gen

data RuneData =
  RuneData { transliteration :: Char
           , name :: Rune
           , meaning :: String
           , unicode :: Char
           , aett :: RuneAett
           , manticDescription :: String -- change to Text
           } deriving (Eq, Show)

data RuneAett = Freyr | Hagall | Tyr deriving (Eq, Show)
type RuneSemiotics = (Char, Char, String)

getDataFor :: Maybe Rune -> RuneData
getDataFor Nothing = error "Not a Futhark rune provided"
getDataFor (Just rune) =
  RuneData { transliteration = latinLetter
           , name = rune
           , meaning = runeMeaning
           , unicode = unicodeRune
           , aett = getAettFor rune
           , manticDescription = ""
           }
  where
    (latinLetter, unicodeRune, runeMeaning) = getSemioticsFor rune

getAettFor :: Rune -> RuneAett
getAettFor rune 
    | rune `elem` [minBound .. Wunjo] = Freyr
    | rune `elem` [Hagalaz .. Sowilo] = Hagall
    | rune `elem` [Tiwaz ..] = Tyr
    | otherwise = error "Not a Futhark rune provided"


getSemioticsFor :: Rune -> RuneSemiotics
getSemioticsFor Fehu = ('f', 'ᚠ', "cattle; wealth")
getSemioticsFor Uruz = ('u', 'ᚢ', "aurochs, wild ox")
getSemioticsFor Thurisaz = ('þ', 'ᚦ', "Thurs, Jötunn")
getSemioticsFor Ansuz = ('a', 'ᚨ', "god, aesir")
getSemioticsFor Raido = ('r', 'ᚱ', "ride, journey")
getSemioticsFor Kaunan = ('k', 'ᚲ', "ulcer, wound, illness")
getSemioticsFor Gebo = ('g', 'ᚷ', "gift")
getSemioticsFor Wunjo = ('w', 'ᚹ', "joy")
getSemioticsFor Hagalaz = ('h', 'ᚺ', "hail")
getSemioticsFor Naudiz = ('n', 'ᚾ', "need")
getSemioticsFor Isaz = ('i', 'ᛁ', "ice")
getSemioticsFor Jera = ('j', 'ᛃ', "year, good year, harvest")
getSemioticsFor Eiwaz = ('æ', 'ᛇ', "yew-tree")
getSemioticsFor Perth = ('p', 'ᛈ', "possibly pear-tree")
getSemioticsFor Algiz = ('z', 'ᛉ', "elk; protection, defence")
getSemioticsFor Sowilo = ('s', 'ᛊ', "sun")
getSemioticsFor Tiwaz = ('t', 'ᛏ', "the god Tiwaz (Týr)")
getSemioticsFor Berkanan = ('b', 'ᛒ', "birch")
getSemioticsFor Ehwaz = ('e', 'ᛖ', "horse")
getSemioticsFor Mannaz = ('m', 'ᛗ', "man")
getSemioticsFor Laguz = ('l' , 'ᛚ', "water, lake")
getSemioticsFor Ingwaz = ('ŋ', 'ᛜ', "the ancestor god Ingwaz (Freyr)")
getSemioticsFor Othala = ('o', 'ᛟ', "heritage, estate, possession")
getSemioticsFor Dagaz = ('d', 'ᛞ', "day")

{-# LANGUAGE QuasiQuotes #-}

module Rune ( Rune
            , RuneData(..)
            , getDataFor
            ) where

import Data.String.Here (here)
import System.Random (Random, random, randomR)

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
  random = randomR (minBound, maxBound)

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


divinationFehu =
  [here|
       Fehu is a rune of wealth, fortune, success.
       Overall, it describes material gains, even in the context of spiritual concerns.
       Besides, as the first rune of Futhark, Fehu emphasizes new beginning, innovation, progress.
       Fehu advises to focus on your business, pointing out now is a good time to undertake material-world operations.
       |]

divinationUruz =
  [here|
       Uruz stands for aurochs and suggests connection to wild strength, natural powers and elemental energies.
       The rune could predict that uncontrolled power enters your life and leads to change.
       Uruz recommends to embrace solid external influence and the new reality it brings.
       |]

divinationThurisaz =
  [here|
       Thurs are Giants or, more widely, "the Other" of the Northern myths.
       They are powerful beings who often oppose Gods and Heroes.
       Thus, the rune may outline serious challenge, which shouldn't be taken lightly.
       Nevertheless, once the challenge is overcome, you get precious wisdom, skill, possession.
       |]

divinationAnsuz =
  [here|
       Ansuz is a rune of Gods Aesir, who rule the world, namely Odin.
       It relates to spiritual and mystical forces as well as intellectual gains.
       Ansuz offers to pay attention to developing your skill, becoming a better version of yourself and to self-realization in general.
       |]

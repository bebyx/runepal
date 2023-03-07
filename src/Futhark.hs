{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Futhark ( Rune
               , RuneData(..)
               , futhark
               , getDataFor
               ) where

import Data.Aeson (defaultOptions, genericToEncoding, toEncoding, ToJSON)  
import Data.String.Here.Uninterpolated (here)
import GHC.Generics (Generic)
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
          | Dagaz
          | Othala deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic)

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
           , divination :: String
           } deriving (Eq, Show, Generic)

data RuneAett = Freyr | Hagall | Tyr deriving (Eq, Show, Generic)
type RuneSemiotics = (Char, Char, String, String)

-- | JSON generic instantiation
instance ToJSON Rune where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON RuneData where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON RuneAett where
  toEncoding = genericToEncoding defaultOptions


futhark :: [Rune]
futhark = [minBound .. ]

getDataFor :: Rune -> RuneData
getDataFor rune =
  RuneData { transliteration = latinLetter
           , name = rune
           , meaning = runeMeaning
           , unicode = unicodeRune
           , aett = getAettFor rune
           , divination = divinationString
           }
  where
    (latinLetter, unicodeRune, runeMeaning, divinationString) = getSemioticsFor rune

getAettFor :: Rune -> RuneAett
getAettFor rune 
    | rune `elem` [minBound .. Wunjo] = Freyr
    | rune `elem` [Hagalaz .. Sowilo] = Hagall
    | rune `elem` [Tiwaz ..] = Tyr
    | otherwise = error "Not a Futhark rune provided"


getSemioticsFor :: Rune -> RuneSemiotics
getSemioticsFor Fehu =
  ('f', 'ᚠ', "cattle; wealth",
    [here|
         Fehu is a rune of wealth, fortune, success. Overall, it describes material gains, even in the context of spiritual concerns. Besides, as the first rune of Futhark, Fehu emphasizes new beginning, innovation, progress. Fehu advises to focus on your business, pointing out now is a good time to undertake material-world operations.
         |])
getSemioticsFor Uruz =
  ('u', 'ᚢ', "aurochs, wild ox",
    [here|
         Uruz stands for aurochs and suggests connection to wild strength, natural powers and elemental energies. The rune could predict that uncontrolled power enters your life and leads to change. Uruz recommends to embrace solid external influence and the new reality it brings.
         |])
getSemioticsFor Thurisaz =
  ('þ', 'ᚦ', "Thurs, Jötunn",
    [here|
         Thurs are Giants or, more widely, "the Other" of the Northern myths. They are powerful beings who often oppose Gods and Heroes. Thus, the rune may outline serious challenge, which shouldn't be taken lightly. Nevertheless, once the challenge is overcome, you get precious wisdom, skill, possession.
         |])
getSemioticsFor Ansuz =
  ('a', 'ᚨ', "god, As",
    [here|
         Ansuz is a rune of Gods Aesir, who rule the world, namely Odin. It relates to spiritual and mystical forces as well as intellectual gains. Ansuz offers to pay attention to developing your way of life, becoming a better version of yourself and to self-realization in general.
         |])
getSemioticsFor Raido =
  ('r', 'ᚱ', "ride, journey",
    [here|
         Raido means travel and empowers anything that is a movement. If you are in the middle of some process, just continue. Try to forget about the destination and enjoy the process itself.
         |])
getSemioticsFor Kaunan =
  ('k', 'ᚲ', "ulcer, wound, illness; torch",
    [here|
         Like a wound transforms into a scar once burnt by a fire of a torch, Kaunan is a rune of transformation. Transformation is usually accompanied by discomfort if not pain. That's the essence of the process, when you need to desintegrate the old to get a fresh lively entity. Kaunan suggests to endure the illness of obstacles so strong immunity can arise.
         |])
getSemioticsFor Gebo =
  ('g', 'ᚷ', "gift",
    [here|
         Gebo is a light rune of a gift with a connotation of generousness. This rune outlines the importance of genuine connections between people. It reminds to stay open-minded, hospitable and cultivate the community. If you ask for advice, Gebo puts it clear: look closely at human beings around.
         |])
getSemioticsFor Wunjo =
  ('w', 'ᚹ', "joy",
    [here|
         Wunjo, joy, is a straightforward rune with obvious meaning.  Of course, it's not of cosmic scale, but accentuates a simple mode of life. Whatever bothers you, remember that you can breathe fresh air, eat tasty food and talk to kind people. Grandiose stuff is so small, when you consider the power of detail.
         |])
getSemioticsFor Hagalaz =
  ('h', 'ᚺ', "hail",
    [here|
         Hagalaz, hail, is a really severe, even brutal act of Nature. However, as every elemental necessity, it brings some positives with itself. Most probably Hagalaz denotes a harsh event in your life. Still, you can take cover and go through it to see the rainbow of opportunity in the rebooted sky.
         |])
getSemioticsFor Naudiz =
  ('n', 'ᚾ', "need",
    [here|
         Need, hardship, distress -- this is what Naudiz uncovers. The rune can indeed signal about dangers of the external world. But despite of being unpleasant, they not lethal. Fear not, work hard and remember that necessity is the mother of invention.
         |])
getSemioticsFor Isaz =
  ('i', 'ᛁ', "ice",
    [here|
         Isaz literally means ice and typifies statics. The rune points that life enters a period of winter, when Nature falls asleep til spring. It's good to find a balance with the sleepy slow season and avoid any turmoil.
         |])
getSemioticsFor Jera =
  ('j', 'ᛃ', "year, good year, harvest",
    [here|
         Jera predicts harvest of fruits of your previous acts. Usually this means you will get something sweet and nutricious. However, if one didn't work hard nor smart, the harvest could be poor. Jera reminds to recall the cyclic essence of life and always consider consequences.
         |])
getSemioticsFor Eiwaz =
  ('æ', 'ᛇ', "yew-tree",
    [here|
         Evergreen yew-tree is flexible and very durable. Eiwaz shows up when one found the proper target. Feel free to take a branch to make a bow and shoot.
         |])
getSemioticsFor Perth =
  ('p', 'ᛈ', "cup; game box; pear-tree, fruit",
    [here|
         Perth is a rune of pleasure. It suggests that a pleasant period awaits and there's no need to resist temptation. However, don't fall into the trap of hedonism, share your pleasures with friends.
         |])
getSemioticsFor Algiz =
  ('z', 'ᛉ', "elk; protection, defence",
    [here|
         Algiz, elk, is a rune that stands for protection. Elk is a calm animal, but big, strong and with spiky horns. Elk wouldn't attacks first, but would certainly strike back as needed. Whatever happens, it reminds to be cautious and take great care.
         |])
getSemioticsFor Sowilo =
  ('s', 'ᛊ', "sun",
    [here|
         Sowilo eponymize powerful energy of the sun. This energy is life-giving and highly dynamic, it breaks the statics of winter. The rune might manifest that it's time to melt the ice and speed up.
         |])
getSemioticsFor Tiwaz =
  ('t', 'ᛏ', "the god Tiwaz (Týr)",
    [here|
         Tiwaz (Týr) is a one-handed battle-god. Norsemen engraved swords with T-runes and it says it all. Prepare for war, train to win a victory.
         |])
getSemioticsFor Berkanan =
  ('b', 'ᛒ', "birch",
    [here|
         Berkanan, literally birch, is a rune of female energy and fertility. It could tell you that beauty will be faced and passion will be experienced. Berkanan encourages to enjoy love, focus maternity and uncover tenderness. 
         |])
getSemioticsFor Ehwaz =
  ('e', 'ᛖ', "horse",
    [here|
         The Horse rune emanates with rapid energy of road, trade, competition. Ehwaz may point out that there will be abrupt change. And there's no need to hide, but to play the game pushed on you. Eat from the exuberant event, ride that horse for your own benefit.
         |])
getSemioticsFor Mannaz =
  ('m', 'ᛗ', "man",
    [here|
         Human minus animal is what Mannaz declares. Human, all too human is walking around and, maybe, it's about time to pay attention to it. The rune can advise to not only turn back to the essentials of intelligence. Remember it's only soul, whatever defined, that stays after the body becomes ashes. 
         |])
getSemioticsFor Laguz =
  ('l' , 'ᛚ', "water, lake",
    [here|
         Laguz means water, or lake, and expresses imagination. It can flow far far away and cover everything under its masses. Maybe, it would be useful to control this powerful element? Channels make a great deal if don't pollute the stream.
         |])
getSemioticsFor Ingwaz =
  ('ŋ', 'ᛜ', "the ancestor god Ingwaz (Freyr)",
    [here|
         Ingwaz represents the first of Danes, who's a great hermit. Personified as Freyr and living in a stranger aett, it stresses solitude even more. Sometimes thoughts become much clear when you give them time to thrive. The rune might whisper that you should consider isolation and retreat.
         |])
getSemioticsFor Dagaz =
  ('d', 'ᛞ', "day",
    [here|
         Dagaz is a light rune of Day an its perk. Day is the most valuable time of human activity. Day brings clearness to business and put away dusk and darkness. The rune offers to focus on daily duties to achieve goals and upgrade the routine.
         |])
getSemioticsFor Othala =
  ('o', 'ᛟ', "heritage, estate, possession",
    [here|
         Othala stands for home, heritage -- possessions that are not merely materialistic. The rune strongly reflects ancestry and invaluable entities like family, kin, culture. The main message of Othala is to maintain your house and cherish the tradition.
         |])

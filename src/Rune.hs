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
           , divination :: String -- change to Text
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
           , divination = ""
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
getSemioticsFor Kaunan = ('k', 'ᚲ', "ulcer, wound, illness; torch")
getSemioticsFor Gebo = ('g', 'ᚷ', "gift")
getSemioticsFor Wunjo = ('w', 'ᚹ', "joy")
getSemioticsFor Hagalaz = ('h', 'ᚺ', "hail")
getSemioticsFor Naudiz = ('n', 'ᚾ', "need")
getSemioticsFor Isaz = ('i', 'ᛁ', "ice")
getSemioticsFor Jera = ('j', 'ᛃ', "year, good year, harvest")
getSemioticsFor Eiwaz = ('æ', 'ᛇ', "yew-tree")
getSemioticsFor Perth = ('p', 'ᛈ', "cup; game box; pear-tree, fruit")
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

divinationRaido =
  [here|
       Raido means travel and empowers anything that is a movement.
       If you are in the middle of some process, just continue.
       Try to forget about the destination and enjoy the process itself.
       |]

divinationKaunan =
  [here|
       Like a wound transforms into a scar once burnt by a fire of a torch, Kaunan is a rune of transformation.
       Transformation is usually accompanied by discomfort if not pain.
       That's the essence of the process, when you need to desintegrate the old to get a fresh lively entity.
       Kaunan suggests to endure the illness of obstacles so strong immunity can arise.
       |]

divinationGebo =
  [here|
       Gebo is a light rune of a gift with a connotation of generousness.
       This rune outlines the importance of genuine connections between people.
       It reminds to stay open-minded, hospitable and cherish the community.
       If you ask for advice, Gebo puts it clear: look closely at human beings around.
       |]

divinationWunjo =
  [here|
       Wunjo, joy, is a straightforward rune with obvious meaning.
       Of course, it's not of cosmic scale, but accentuates a simple mode of life.
       Whatever bothers you, remember that you can breathe fresh air, eat tasty food and talk to kind people.
       Grandiose stuff is so small, when you consider the power of detail.
       |]

divinationHagalaz =
  [here|
       Hagalaz, hail, is a really severe, even brutal act of Nature.
       However, as every elemental necessity, it brings some positives with itself.
       Most probably Hagalaz denotes a harsh event in your life.
       Still, you can take cover and go through it to see the rainbow of opportunity in the rebooted sky.
       |]

divinationNaudiz =
  [here|
       Need, hardship, distress -- this is what uncovers Naudiz.
       The rune can indeed signal about dangers of the external world.
       But despite of being unpleasant, they not lethal.
       Fear not, work hard and remember that necessity is the mother of invention.
       |]

divinationIsaz =
  [here|
       Isaz literally means ice and typifies statics.
       The rune points that life enters a period of winter, when Nature falls asleep til spring.
       It's good to find a balance with the sleepy slow season and avoid any turmoil.
       |]

divinationJera =
  [here|
       Jera predicts harvest of fruits of your previous acts.
       Usually this means you will get something sweet and nutricious.
       However, if one didn't work hard nor smart, the harvest could be poor.
       Jera reminds to recall the cyclic essence of life and always consider consequences.
       |]

divinationEiwaz =
  [here|
       Evergreen yew-tree is flexible and very durable.
       Eiwaz shows up when one found the proper target.
       Feel free to take a branch to make a bow and shoot.
       |]

divinationPerth =
  [here|
       Perth is a rune of pleasure.
       It suggests that a pleasant period awaits and there's no need to resist temptation.
       However, don't fall into the trap of hedonism, share your pleasures with friends.
       |]

divinationAlgiz =
  [here|
       Algiz, elk, is a rune that stands for protection.
       Elk is a calm animal, but big, strong and with spiky horns.
       Elk wouldn't attacks first, but would certainly strike back as needed.
       Whatever happens, it reminds to be cautious and take great care.
       |]

divinationSowilo =
  [here|
       
       |]

divinationTiwaz =
  [here|
       
       |]

divinationBerkanan =
  [here|
       
       |]

divinationEhwaz =
  [here|
       
       |]

divinationMannaz =
  [here|
       
       |]

divinationLaguz =
  [here|
       
       |]

divinationIngwaz =
  [here|
       
       |]

divinationOthala =
  [here|
       
       |]

divinationDagaz =
  [here|
       
       |]

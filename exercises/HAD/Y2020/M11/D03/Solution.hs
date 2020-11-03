{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D03.Solution where

-- Looking at the solution set of:

import Y2020.M10.D30.Solution

-- I have questions.

-- first, let's load and ... 'semi'-correct (?) those data:

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

import Y2020.M10.D12.Solution hiding (workingDir)     -- for Country
import Y2020.M10.D14.Solution                         -- for ContinentMap
import Y2020.M10.D28.Solution hiding (Alliance, name, countries) -- for Name

militaryAlliances :: IO (Map Name Alliance)
militaryAlliances =
   parseAlliances (dear ++ moderns)         >>= \allis0 ->
   countriesByContinent (workingDir ++ cbc) >>= \m ->
   let uau = updateAfricanUnion m "Africa"
                    (Alliance "African Union" Set.empty Set.empty)
   in  return (Map.insert "African Union" uau allis0)

{--
>>> militaryAlliances
...
>>> let ma = it

Looking at this map, we have an alliance, like:

>>> last (Map.elems ma)
Alliance {name = "Union State", aliases = fromList [], countries = fromList []}

An alliance of no countries. There's more than one of these. How many 'empty'
alliances are there? What are these alliances?
--}

emptyAlliances :: AllianceMap -> Set Name
emptyAlliances = Map.keysSet . Map.filter ((== Set.empty) . countries)

{--
>>> let empties = emptyAlliances ma
>>> empties
fromList ["2001 Sino-Russian Treaty of Friendship","ABCA Armies",
          "Agreement on Strategic Partnership and Mutual Support",
          "Axis of Resistance","File:Coat of Arms of TAKM.jpg|22px",
          "File:GUAM logo.png|36px","File:NATO flag.svg|22px",
          "Forum for the Progress and Development of South America",
          "International Maritime Security Construct",
          "Islamic Military Alliance","Major non-NATO Ally",
          "Multinational Force and Observers",
          "South Atlantic Peace and Cooperation Zone","Union State"]

>>> Set.size empties
14
--}

{--
The problem that created these empty alliances could be a parsing one, because
some person MIGHT have changed the data format mid-way of how an alliance is
listed, arbitrarily, because this person wanted to make my life less boring.

Thank you, some person. No, really, you're doing a great job. Love your work.

So, our task specification is a simple one:

1. determine what this new, funky format is
2. write a new, funky parser for this new alliance-format
3. go back to the file with these empty alliances and this new, funky parser
4. update the alliance map with the now-populated alliances

newFunkyParser :: FilePath -> IO AllianceMap
newFunkyParser = undefined

newFunkyAllianceMap :: AllianceMap -> AllianceMap -> AllianceMap
newFunkyAllianceMap = undefined

... actually ... the below code is pointing in the direction of refinement,
post-parsing.
--}

{--
Refine newFunkyAllianceMap until you have no empty alliances or until you want
to pull your hair out and stomp up and down, up and down and run around the
house, screaming, ...

... whichever comes first.

With this new, funky alliance map, answer the questions from the previous 
exercise. Which answers have changed?
--}

type AA = AllianceMap -> AllianceMap

refinements :: AA
refinements = sinoRussian . fiveEyes . turkeyAzerbaijan . takm . guam. nato

sinoRussian :: AA
sinoRussian = let treat =  "2001 Sino-Russian Treaty of Friendship" in
              newFunkyInsert treat (T.words "Russia China")

fiveEyes :: AA
fiveEyes = Map.delete "ABCA Armies"
         . newFunkyInsert "Five Eyes"
                     ("New Zealand":T.words "UK USA Canada Australia")

turkeyAzerbaijan :: AA
turkeyAzerbaijan =
   let name = "Agreement on Strategic Partnership and Mutual Support"
   in  newFunkyInsert name (T.words "Turkey Azerbaijan")

{--
Look at this one:

[[Axis of Resistance]] {{Flagicon|Iran}} {{flagicon|Syria}} 
         {{Flagicon|Hezbollah}} {{Flagicon image|Ansarullah Flag Vector.svg}}
         {{Flagicon image|Iraqi PMF Flag.svg}}

We have '(F)lagicon' and '(f)lagicon' ... Text-type-checking, anyone?

Also we introduce a new type: 'Flagicon image' ... which, it appears, only the
first word is relevant? We shall see.

So, how do I fix this? Aspects?

... eh.

I'm just going to go back to the previous exercise and fix it there.

So, rerunning the system with the changes in the previous exercise gives:

>>> militaryAlliances 
>>> let ma = it
>>> Map.lookup (T.pack "Axis of Resistance") ma
Just (Alliance {name = "Axis of Resistance", aliases = fromList [],
       countries = fromList ["Ansarullah","Hezbollah","Iran","Iraqi","Syria"]})

WOOT!

Okay, moving along!
--}

takm :: AA
takm = let longNameSheesh =
                  T.concat ["Organization of the Eurasian Law ",
                            "Enforcement Agencies with Military Status"] in
     Map.delete "File:Coat of Arms of TAKM.jpg|22px"
     . newFunkyInsert1 ["TAKM"] longNameSheesh
                       (T.words "Turkey Azerbaijan Mongolia Kyrgyzstan")

guam :: AA
guam = let longie = "GUAM Organization for Democracy and Economic Development"
           wrongie = "File:GUAM logo.png|36px"
       in  Map.delete wrongie
         . newFunkyInsert1 ["GUAM"] longie
                           (T.words "Georgia Ukraine Azerbaijan Moldova")


natoFlags :: String
natoFlags = concat ["{{flagicon|USA}} {{flagicon|UK}} ",
      "{{flagicon|France}} {{flagicon|Canada}} {{flagicon|Belgium}} ",
      "{{flagicon|Denmark}} {{flagicon|Iceland}} {{flagicon|Italy}} ",
      "{{flagicon|Luxembourg}}  {{flagicon|Netherlands}} ",
      "{{flagicon|Norway}} {{flagicon|Portugal}} {{flagicon|Greece}} ",
      "{{flagicon|Turkey}} {{flagicon|Germany}} {{flagicon|Spain}} ",                 "{{flagicon|Czech Republic}} {{flagicon|Hungary}} ",
      "{{flagicon|Poland}} {{flagicon|Bulgaria}} ",
      "{{flagicon|Estonia}} {{flagicon|Latvia}} ",
      "{{flagicon|Lithuania}} {{flagicon|Romania}} ",
      "{{flagicon|Slovakia}} {{flagicon|Slovenia}} ",
      "{{flagicon|Albania}} {{flagicon|Croatia}} ",
      "{{flagicon|Montenegro}} {{flagicon|North Macedonia}}"]

nato :: AA
nato = let neato = "North Atlantic Treaty Organization"
           icko  = "File:NATO flag.svg|22px"
       in  Map.delete icko
         . Map.insert neato (Alliance neato (Set.singleton "NATO") cf)
   where cf = fromJust (countryFlags natoFlags)

-- coffee-break!

reviseAlliance :: Name -> [Text] -> Alliance
reviseAlliance = reviseAlliance1 []

newFunkyInsert :: Name -> [Text] -> AA
newFunkyInsert = newFunkyInsert1 []

newFunkyInsert1 :: [Alias] -> Name -> [Text] -> AA
newFunkyInsert1 aliases n allies =
   Map.insert n (reviseAlliance1 aliases n allies)

reviseAlliance1 :: [Alias] -> Name -> [Text] -> Alliance
reviseAlliance1 ali n = Alliance n (Set.fromList ali) . Set.fromList

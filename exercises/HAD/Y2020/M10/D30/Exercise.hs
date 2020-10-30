{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D30.Exercise where

{--
So, the wikipedia entry for all alliances of the world:

https://en.wikipedia.org/wiki/List_of_military_alliances

has a section on modern, or current, military alliances. These entries
are saved in the wikitext-format. The wikitext format is described here:

https://meta.wikimedia.org/wiki/Help:Wikitext_examples

... but is self-evident from the text, itself.

The listing of modern military alliances is archived here:
--}

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

import Y2020.M10.D12.Exercise     -- for Country
import Y2020.M10.D14.Exercise     -- for ContinentMap
import Y2020.M10.D28.Exercise hiding (Alliance)    -- for Name

dear :: FilePath
dear = "Y2020/M10/D30/"    -- ... geddit?

moderns :: FilePath
moderns = "modern-alliances.wtxt"

-- figure out how to scan and parse the above document into the below
-- data structure

type Alias = Text

data Alliance = Alliance Name (Set Alias) (Set Country)
   deriving (Eq, Ord, Show)

type AllianceMap = Map Name Alliance

parseAlliances :: String -> AllianceMap
parseAlliances wikitext = undefined

{-- 
Parsing alliances should be (once you get to an alliance block):

parseStar
parseOptionalSpace
optionallyParseAllianceFlag
parseAllianceName
parseEachAllianceCountry
optionallyParseAliases (on the new lines, designated by **)

With that, you should be able to retrieve each alliance.

Except: --v
--}

-- data clean-up --------------------------------------------------

{--
Okay, very first alliance, we have a problem. "African Union: all 58 African
nations are members."

What are the 58 African nations? Replace the free text with those nations.

Use the ContinentMap to guide you.
--}

updateAfricanUnion :: ContinentMap -> Alliance -> Alliance
updateAfricanUnion = undefined

-- Okay. Now that we have the modern alliances

-- 1. How many alliances are there?

-- 2.a. How many countries are in multiple alliances?
-- 2.b. list those countries with their multiple alliances

-- 3. What Countries here are not in the CountryMap?

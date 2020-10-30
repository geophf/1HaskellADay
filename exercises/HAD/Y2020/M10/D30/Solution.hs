{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D30.Solution where

{--
So, the wikipedia entry for all alliances of the world:

https://en.wikipedia.org/wiki/List_of_military_alliances

has a section on modern, or current, military alliances. These entries
are saved in the wikitext-format. The wikitext format is described here:

https://meta.wikimedia.org/wiki/Help:Wikitext_examples

... but is self-evident from the text, itself.

The listing of modern military alliances is archived here:
--}

import Control.Arrow ((&&&), first)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import Data.Text (Text)

import Y2020.M10.D12.Solution     -- for Country
import Y2020.M10.D14.Solution     -- for ContinentMap
import Y2020.M10.D28.Solution hiding (Alliance, name)    -- for Name

dear :: FilePath
dear = "Y2020/M10/D30/"    -- ... geddit?

moderns :: FilePath
moderns = "modern-alliances.wtxt"

-- figure out how to scan and parse the above document into the below
-- data structure

type Alias = Text

data Alliance = Alliance { name :: Name, 
                           aliases :: Set Alias,
                           countries :: Set Country }
   deriving (Eq, Ord, Show)

type AllianceMap = Map Name Alliance

parseAlliances :: FilePath -> IO AllianceMap

{--
The mapMaybe approach doesn't work, because we aren't accepting or rejecting
alliances BY LINE, because aliases follow the alliance, line-by-line. So,
we need to parse an alliance on this line, then keep that context open to
parse, possibly, any aliases the alliance may have.

parseAlliances wikitext = 
   Map.fromList . map (name &&& id) . mapMaybe pa' . lines <$> readFile wikitext

pa' :: String -> Maybe Alliance
pa' = undefined
--}

-- (nice try, though. Come again, soon.)

parseAlliances wikitext = 
   Map.fromList . map (name &&& id) . pa' [] . lines <$> readFile wikitext

pa' :: [Alliance] -> [String] -> [Alliance]
pa' ans [] = ans
pa' acc l@(_:ines) = 
   maybe (pa' acc ines) (uncurry pa' . first (:acc)) (pa'' l)

pa'' :: [String] -> Maybe (Alliance, [String])
pa'' (l:ines) =
   parseStar l          >>=
   consumeSpaces        >>=
   optionalFlagIcon     >>=
   allianceName         >>= \(n,r0) ->
   countryFlags r0      >>= \flags  ->
   optionalAliases ines >>= 
   return . first (flip (Alliance n) flags)

{-- 
Parsing alliances should be (once you get to an alliance block):

parseStar
parseOptionalSpace
optionallyParseAllianceFlag
parseAllianceName
parseEachAllianceCountry
optionallyParseAliases (on the new lines, designated by **)

With that, you should be able to retrieve each alliance.

--}

parseStar :: String -> Maybe String
parseStar ('*':rest) = Just rest
parseStar _          = Nothing

consumeSpaces :: String -> Maybe String
consumeSpaces = undefined

optionalFlagIcon :: String -> Maybe String
optionalFlagIcon = undefined

allianceName :: String -> Maybe (Text, String)
allianceName = undefined

countryFlags :: String -> Maybe (Set Country)
countryFlags = undefined

optionalAliases :: [String] -> Maybe (Set Alias, [String])
optionalAliases = undefined

-- Except: --v

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

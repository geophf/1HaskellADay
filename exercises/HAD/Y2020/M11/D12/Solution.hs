{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D12.Solution where

-- Well, howdy! Yesterday, ...

import Y2020.M11.D11.Solution (allianceGraph)

{--
... we were all: "We're gonna upload alliances to the graph-store!"

But we didn't because the day ended in an unicode-error. *sad-face*

So, today, let's:

1. first, identify the values that have unicode-points outside ASCII
2. then, replace those values with plain-vanilla ASCII values
3. finally, upload the newly updated AllianceMap to the graph
   (which we tried yesterday, so this step should be easy).
--}

import Y2020.M10.D23.Solution (unicodePoints)
import Y2020.M10.D30.Solution             -- for AllianceMap
import Y2020.M11.D10.Solution (go)

import Graph.Query

import Control.Arrow (second)

import Data.Char (ord)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

-- for Step 1, look at, e.g. Y2020.M10.D23.Solution.stripNonAscii

isNonAscii :: Text -> Bool
isNonAscii = any ((> 127) . ord) . T.unpack

-- How many thingies (that's a technical term) are non-ascii?

-- for Step 2, we can do this generically or we can do it specifically.

cleanText :: Text -> Text
cleanText = T.concat . unicodePoints

-- `cleanText` given text isNonAscii replace it with Text value that is ASCII

-- So, then, we cleanify (that's a word, now) the AllianceMap thusly:

-- ... but first: I want to see what works and what doesn't UTF8-wise

whatify :: AllianceMap -> IO AllianceMap
whatify am = Map.fromList <$> mapM (sequence . second ally) (Map.toList am)

ally :: Alliance -> IO Alliance
ally a = 
   putStrLn (concat ["For ", T.unpack $ name a, ":"]) >>
   let b = Set.filter isNonAscii (countries a) in
   putStrLn (show b) >>
   return a { countries = b }

{--
>>> whatify am
For 2001 Sino-Russian Treaty of Friendship:
fromList []
For ANZAC:
fromList []
For ANZUS:
fromList []
For African Union:
fromList ["C\244te d'Ivoire (Ivory Coast)","S\227o Tom\233 and Pr\237ncipe"]
For Agreement on Strategic Partnership and Mutual Support:
fromList []
For Anglo-Portuguese Alliance:
fromList []
For Arab League:
fromList []
For Association of South-East Asian Nations:
fromList []
For Axis of Resistance:
fromList []
For BALTRON:
fromList []
For Caribbean Community|Caribbean Community (CARICOM):
fromList []
For Collective Security Treaty Organization:
fromList []
For Economic Community of Central African States:
fromList []
For Economic Community of West African States:
fromList []
For Entente Cordiale:
fromList []
For European Union:
fromList []
For Five Eyes:
fromList []
For Five Power Defence Arrangements:
fromList []
For Forum for the Progress and Development of South America:
fromList []
For Franco-German Brigade:
fromList []
For GUAM Organization for Democracy and Economic Development:
fromList []
For International Association of Gendarmeries and Police Forces with Military Status:
fromList []
For International Maritime Security Construct:
fromList []
For Islamic Military Alliance:
fromList []
For Major non-NATO Ally:
fromList []
For Moroccan-American Treaty of Friendship:
fromList []
For Mutual Defense Treaty (United States-Philippines):
fromList []
For Mutual Defense Treaty (United States-South Korea):
fromList []
For North American Aerospace Defense Command:
fromList []
For North Atlantic Treaty Organization:
fromList []
For Organization of American States:
fromList []
For Organization of the Eurasian Law Enforcement Agencies with Military Status:
fromList []
For Pakistan-United States military relations:
fromList []
For Peninsula Shield Force:
fromList []
For Rio Pact:
fromList []
For Russia-Syria-Iran-Iraq Coalition|RSII Coalition:
fromList []
For Shanghai Cooperation Organisation:
fromList []
For Sino-North Korean Mutual Aid and Cooperation Friendship Treaty:
fromList []
For South Atlantic Peace and Cooperation Zone:
fromList []
For Treaty of Mutual Cooperation and Security between the United States and Japan:
fromList []
For U.S.-Afghanistan Strategic Partnership Agreement:
fromList []
For US-Israel Strategic Partnership:
fromList []
For Union State:
fromList []
For Union of South American Nations:
fromList []
For United Nations:
fromList []

... so it's a rare issue, not a profligate one. Let's examine the cases, and
see how the countries are represented in the graph-store.

match (n:Continent {name: "Africa"})<-[:IN]-(c:Country) return c.name

São Tomé and Príncipe
Côte d'Ivoire (Ivory Coast)

Okay, the what? :<

Shuckaroos! I'm just going to drop those two countries.
--}

cleanify :: AllianceMap -> AllianceMap
cleanify = Map.fromList . map (second allu) . Map.toList

allu :: Alliance -> Alliance
allu a =
   let b = Set.filter (not . isNonAscii) (countries a) in
   a { countries = b }

-- now upload the alliances to the graph (as yesterday).

{--
>>> go
>>> let am = it
>>> let cleaned = cleanify am
>>> graphEndpoint 
>>> let url = it
>>> cyphIt url (allianceGraph cleaned)
...,\"errors\":[]}"

--}

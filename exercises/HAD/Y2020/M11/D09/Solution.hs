{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D09.Solution where

{-- 
The previous exercise had you parse the European Union's member states then
add that to the AllianceMap of the world. That was easy, because those data
were available as wikidata.org JSON.

Not so for the United Nations (which we will do today) and the Organization
of American States (tomorrow). These alliances are in wikitext, and, what's
even more FUNZORX is that these wikitext documents don't have an uniform
format.

YAY! FUNZORX!

Today's #haskell problem.

Let's parse in the member nations of the United Nations and add that ...
'alliance' (?) to our AllianceMap.

I'll provide the seed alliance map. You provide the UN and add it to that map.

hint: we may (?) have parsed in nation flags (?) before (?) from wikitext?

post-hint: oh. It's not {{flagicon|<country>}}, now it's {{flag|<country>}}

The Funzorx continuezorx. :/
--}

import Y2020.M10.D12.Solution    -- ALL the way back when for Country.

import Y2020.M10.D30.Solution (Alliance(Alliance), AllianceMap, dear, moderns)
import qualified Y2020.M10.D30.Solution as A  -- look for flaggie stuff here
import Y2020.M11.D05.Solution (todoPrep)    -- for the updated alliance-parse
import Y2020.M11.D06.Solution (addEU, euDir, eu, adder)

import Data.List (stripPrefix)

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import qualified Data.Set as Set

import qualified Data.Text as T

seedAllianceMap :: FilePath -> FilePath -> IO AllianceMap
seedAllianceMap allieses eus = todoPrep allieses >>= flip addEU eus

{--
>>> seedAllianceMap (dear ++ moderns) (euDir ++ eu)
...
>>> let am = it
>>> Map.size am
43
--}

unDir :: FilePath
unDir = "Y2020/M11/D09/"

un :: FilePath
un = "un.wtxt"

unFlag :: String -> Maybe Country
unFlag line = T.pack . fst . break (== '}')
    <$> A.prolog [stripPrefix "|{{flag|", stripPrefix "|{{Flag|"] line

-- unFlag is the same flagicon, but for "{{flag|", ... so it's different. :/

exUnFlagPass, exUnFlagFail :: String
exUnFlagPass = "|{{flag|China}}"
exUnFlagFail = "|1971, replaced the [[Republic of China]]"

{--
>>> unFlag exUnFlagPass 
Just "China"
>>> unFlag exUnFlagFail
Nothing
--}

-- and with that --^ we can do this --v

unitedNationsParser :: FilePath -> IO Alliance
unitedNationsParser file =
   Alliance "United Nations" Set.empty . Set.fromList . mapMaybe unFlag . lines
            <$> readFile file

{--
>>> unitedNationsParser (unDir ++ un)
Alliance {name = "United Nations", aliases = fromList [], 
          countries = fromList ["Belgium","China","Dominican Republic","Estonia",
                            "France","Germany","Indonesia","Niger","Russia",
                            "Saint Vincent and the Grenadines","South Africa",
                            "Tunisia","United Kingdom","United States","Vietnam"]}
--}

addUN :: AllianceMap -> FilePath -> IO AllianceMap
addUN = adder unitedNationsParser

{--
>>> addUN am (unDir ++ un)
...
>>> let unmap = it
>>> Map.size unmap 
44
--}

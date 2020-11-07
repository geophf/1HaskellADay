{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D09.Exercise where

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
--}

import Y2020.M10.D30.Solution (Alliance(Alliance), AllianceMap, dear, moderns)
import qualified Y2020.M10.D30.Solution as A  -- look for flaggie stuff here
import Y2020.M11.D05.Solution (todoPrep)    -- for the updated alliance-parse
import Y2020.M11.D06.Solution (addEU, euDir, eu)

import qualified Data.Map as Map

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

unitedNationsParser :: FilePath -> IO Alliance
unitedNationsParser file = undefined

addUN :: AllianceMap -> FilePath -> IO AllianceMap
addUN am uns = undefined

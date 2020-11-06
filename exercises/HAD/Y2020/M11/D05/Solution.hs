{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D05.Solution where

-- So, yesterday...

import Y2020.M11.D04.Solution

{--
...we discovered we missed alliances in our first-pass parse.

A LOT of alliances.

OR.
DID.
WE?

Examining these data more closely, we find some of these missed-'...alliances'
(?) aren't alliances at all. What are they?

Let's identify what are alliances, and what aren't.
--}

import Y2020.M10.D28.Solution hiding (Alliance, name, countries) -- for Name
import Y2020.M10.D30.Solution
import Y2020.M11.D03.Solution (militaryAlliances, refinements, cfs, AA, newFunkyInsert, newFunkyInsert1)

import Control.Arrow ((***), (&&&), (>>>))
import Control.Monad (join)

import Data.Char (isUpper)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

alliancesAliases :: Set Name -> (Set Name, Set Alias)
alliancesAliases = Set.partition (not . allCaps)

allCaps :: Name -> Bool
allCaps = all isUpper . T.unpack

{--
`alliancesAliases` partitions these 'missed alliances' into viable alliance
names and names that are aliases or something else.

>>> missingAlliances (dear ++ moderns)
>>> let allis = it
>>> militaryAlliances
>>> let ma0 = it
>>> let ma = refinements ma0
>>> let diff = Set.difference (Map.keysSet allis) (Map.keysSet ma)
>>> let aa = alliancesAliases diff

>>> join (***) Set.size aa
(30,5)

>>> snd aa
fromList ["ANZAC","ANZUS","BALTRON","ECOMIL","ECOMOG"]

Okay, that's (slightly) more manageable, but this points out a lack of 
integrity and validation on my part. I left a BIG-OL' TODO in the original 
problem solution to implement the parseAliases, but I didn't get a 'round tuit.'
Let's do that now.

But first! What Alliances already have aliases?

>>> map (name &&& aliases) . filter ((/== Set.empty) . aliases) $ Map.elems ma
[("Forum for the Progress and Development of South America",fromList ["PROSUR"]),
 ("GUAM Organization for Democracy and Economic Development",fromList ["GUAM"]),
 ("Islamic Military Alliance",fromList ["IMAFT"]),
 ("North Atlantic Treaty Organization",fromList ["NATO"]),
 ("Organization of the Eurasian Law Enforcement Agencies with Military Status",fromList ["TAKM"]),
 ("South Atlantic Peace and Cooperation Zone",fromList ["ZPCAS"])]

Okay, for those aliases above, do we have their corresponding alliances?

ANZAC - nupe, that's just an alliance that is its own alias
ANZUS - same
BALTRON - same
ECOMIL, ECOMOG - aliases for Economic Community of West African States

>>> Map.lookup (T.pack "Economic Community of West African States") ma
Just (Alliance {name = "Economic Community of West African States", 
      aliases = fromList [], 
      countries = fromList ["Benin","Burkina Faso","Cabo Verde","Ghana",
                            "Guinea","Guinea-Bissau","Ivory Coast","Liberia",
                            "Mali","Niger","Nigeria","Senegal","Sierra Leone",
                            "The Gambia","Togo"]})

So, four 'aliases' to fix, but this seems easier by hand.
--}

parseAndAddAliases :: AA
parseAndAddAliases am =
   let afri = "Economic Community of West African States"
       newset = Set.fromList (T.words "ECOMIL ECOMOG")
       am0 = maybe am (\alli -> Map.insert afri (alli { aliases = newset }) am)
                   (Map.lookup afri am)
       anz = "{{flagicon|Australia}} {{flagicon|New Zealand}}"
       anzus = concat [anz, " {{flagicon|USA}}"]
       ally alias = Alliance alias (Set.singleton alias) . cfs
       addAliasedAlly alias = Map.insert alias . ally alias
       am1 = addAliasedAlly "ANZAC" anz am0
       am2 = addAliasedAlly "ANZUS" anzus am1
       balty = "{{flagicon|Estonia}} {{flagicon|Latvia}} {{flagicon|Lithuania}}"
       am3 = addAliasedAlly "BALTRON" balty am2
   in  am3
   -- or: in  (foldr (uncurry Map.insert . (name &&& id)) am0 
                  -- [ally "ANZAC" anz, ally "ANZUS" anzus])

{--
Great, but now rerun the missing alliances-scenario again.

You get the same results (please verify this). Why?

>>> let ma1 = ma
>>> let ma = parseAndAddAliases ma1
>>> Map.size ma
24
>>> let diff = Set.difference (Map.keysSet allis) (Map.keysSet ma)
>>> let aa = alliancesAliases diff
>>> join (***) Set.size aa
(30,3)

... okay, somewhat different, because we've added the two alliances-with-alias-
names to our data-set.

Because, even though we now have aliases integrated into our AllianceMap, we
didn't consider aliases when doing the diff on what we have and what we don't.
Let's do that now.
--}

readyDataSets :: FilePath -> IO (AllianceIndex, AllianceMap)
readyDataSets file = militaryAlliances     >>= \ma0 ->
                     missingAlliances file >>= \allis ->
                     return (allis, parseAndAddAliases (refinements ma0))

missingAlliancesWithAliases :: AllianceIndex -> AllianceMap -> Set Name
missingAlliancesWithAliases roughParse =
   Set.difference (allNames roughParse) . allNames

class AKA a where
   aka :: a -> Set Name

instance AKA Alliance where
   aka = aliases

instance AKA (Set a) where
   aka _ = Set.empty

namesOf :: AKA a => (Name, a) -> Set Name
namesOf = id *** aka >>> uncurry Set.insert

allNames :: AKA a => Map Name a -> Set Name
allNames = Set.unions . map namesOf . Map.toList

{--
>>> readyDataSets (dear ++ moderns) >>= return . uncurry missingAlliancesWithAliases 
fromList ["ABCA Armies",   -- Alias; done
          "Anglo-Portuguese Alliance",   -- done
          "Arab League",  -- the Arab League should have been parsed, but, Lo!
                          -- there's a space after the asterix. Why.
   -- (fixed by adding optional spaces-parse to Y2020.M10.D30.Solution)

          "Caribbean Community|Caribbean Community (CARICOM)",  -- fixed^
          "Collective Security Treaty Organization",            -- fixed^
          "Council for Peace and Security in Central Africa",   -- alias; done
          "Department of Peacekeeping Operations",              -- empty/UN
          "Entente Cordiale",                                   -- done
          "European Union","File:Coat of Arms of TAKM.jpg|22px",-- previously
          "File:GUAM logo.png|36px","File:NATO flag.svg|22px",  -- prev handled
          "Franco-German Brigade",                              -- done
          "Inter-American Defense Board",                       -- alias OAS

-- TODO: Organization of American States
-- TODO: UN / United Nations
-- TODO: European Union

          "Moroccan-American Treaty of Friendship",             -- done
          "Multinational Force and Observers",                  -- alias / UN
          "Mutual Defense Treaty (United States\8211Philippines)|Mutual Defense Treaty",
                                                                -- done
          "Mutual Defense Treaty (United States\8211South Korea)|Mutual Defense Treaty",
                                                                -- done
          "North American Aerospace Defense Command",           -- done
          "Organization of American States",                    -- TODO
          "Pakistan\8211United States military relations|Mutual Defense Assistant Agreement",
                                                                -- done
          "Peninsula Shield Force",                             -- done
          "Shanghai Cooperation Organisation",                  -- done
          "Treaty of Mutual Cooperation and Security between the United States and Japan|security alliance",
                                                                -- done
          "U.S.\8211Afghanistan Strategic Partnership Agreement",  -- done
          "Union of South American Nations",                    -- done
          "Union of South American Nations#Defense policy|South American Defense Council",
                                                                -- dup
          "United Nations","United Nations Security Council",   -- TODO/UN
          "United States-Israel Strategic Partnership Act of 2013|US-Israel Strategic Partnership"]

>>> let missies = it
>>> Set.size missies
30

The three 'Flagged'-alliances in the above set have previously been handled,
leaving 27 missing alliances to consume.

Okay, great!

Now let's find a way to parse in these missing alliances, either with a parser
on the raw data, or with some hand-tuned modifications.
--}

incorporateMissingAlliances :: AA
incorporateMissingAlliances = 

   -- digging into the text we find:

   let aasic = "Air and Space Interoperability Council" in
     addAliasesTo "Five Eyes" ["ABCA Armies", aasic, "AUSCANNZUKUS"]
   . addAliasesTo "Economic Community of Central African States"
            ["Council for Peace and Security in Central Africa", "COPAX"]
   . newFunkyInsert "Anglo-Portuguese Alliance" (T.words "Portugal UK")
   . newFunkyInsert1 ["Defence and Security Co-operation Treaty"]
            "Entente Cordiale" (T.words "France UK")
   . newFunkyInsert "Franco-German Brigade" (T.words "France Germany")
   . newFunkyInsert "Moroccan-American Treaty of Friendship"
                    (T.words "Morocco USA")
   . mdt "Philippines" 
   . mdt "South Korea"
   . newFunkyInsert1 ["Mutual Defense Assistant Agreement"]
           "Pakistan-United States military relations" (T.words "Pakistan USA")
   . addAlly "Peninsula Shield Force" persianGulf
   . addAlly1 (Set.singleton "SCO") "Shanghai Cooperation Organisation" shang
   . newFunkyInsert1 ["Security Alliance"] 
            "Treaty of Mutual Cooperation and Security between the United States and Japan"
            ["USA", "Japan"]
   . newFunkyInsert "U.S.-Afghanistan Strategic Partnership Agreement"
            ["USA", "Afghanistan"]
   . newFunkyInsert1 ["UNASUR"] "Union of South American Nations"
            (T.words "Venezuela Uruguay Bolivia Guyana Suriname")
   . newFunkyInsert "US-Israel Strategic Partnership" ["USA", "Israel"]
   . newFunkyInsert1 ["NORAD"] "North American Aerospace Defense Command"
                   ["USA", "Canada"]

   -- three empties deal with here:

   . addAlly "Collective Security Treaty Organization" csto
   . Map.delete "Organization of American States"
   . Map.delete "United Nations"


persianGulf :: String
persianGulf = concat ["{{flagicon|Saudi Arabia}} {{flagicon|UAE}} ",
                      "{{flagicon|Qatar}} {{flagicon|Oman}} ",
                      "{{flagicon|Kuwait}} {{flagicon|Bahrain}}"]

shang :: String
shang = concat ["{{flagicon|China}} {{flagicon|Russia}} {{flagicon|India}} ",
       "{{flagicon|Kazakhstan}} {{flagicon|Kyrgyzstan}} {{flagicon|Tajikistan}} ",
       "{{flagicon|Uzbekistan}} {{flagicon|Pakistan}}"]

csto :: String
csto = concat ["{{flagicon|Armenia}} {{flagicon|Belarus}} ",
               "{{flagicon|Kazakhstan}} {{flagicon|Kyrgyzstan}} ",
               "{{flagicon|Russia}} {{flagicon|Tajikistan}}"]

mdt :: Name -> AA
mdt avec =
   let n = T.concat ["Mutual Defense Treaty (United States-", avec, ")"]
   in  newFunkyInsert n ["USA", avec]

addAliasesTo :: Name -> [Alias] -> AA
addAliasesTo n als ma =
   maybe ma (\a -> Map.insert n (a { aliases = Set.fromList als }) ma)
         (Map.lookup n ma)

addAlly :: Name -> String -> AA
addAlly = addAlly1 Set.empty

addAlly1 :: Set Alias -> Name -> String -> AA
addAlly1 alia n = Map.insert n . Alliance n alia . cfs

{--
How many alliances do we have now? Also, check for empties. Do we have any
empty (that is: no country) alliances?

>>> readyDataSets (dear ++ moderns)
>>> let (missies, ma0) = it
>>> let ma = incorporateMissingAlliances ma0
>>> let mawa = missingAlliancesWithAliases missies ma
>>> Set.size mawa
15

... but, as you can see, these 15 alliances are renames, TODOs, or empties

... except:

>>> emptyAlliances ma
fromList ["Collective Security Treaty Organization",
          "Organization of American States","United Nations"]

which are 2 TODOs but the Collective Security Treaty Organization does need
to be rolled in, and I do that, above, as well as remove the two empties

After rolling in those changes (above), we now have:

>>> emptyAlliances ma
fromList []

So:
--}

todoPrep :: FilePath -> IO AllianceMap
todoPrep file = incorporateMissingAlliances . snd <$> readyDataSets file

-- sets up the AllianceMap for accepting the EU, UN, and the Organization
-- of American States.

{--
>>> todoPrep (dear ++ moderns)
>>> let prep = it
>>> Map.size prep
42
--}
                

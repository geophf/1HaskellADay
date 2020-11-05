{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D05.Exercise where

-- So, yesterday...

import Y2020.M11.D04.Exercise

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

import Y2020.M10.D28.Exercise hiding (Alliance, name, countries) -- for Name
import Y2020.M10.D30.Exercise

import Data.Set (Set)

alliancesAliases :: Set Name -> (Set Name, Set Alias)
alliancesAliases = undefined

{--
`alliancesAliases` partitions these 'missed alliances' into viable alliance
names and names that are aliases or something else.

Okay, that's more manageable, but this points out a lack of integrity and
validation on my part. I left a BIG-OL' TODO in the original problem solution
to implement the parseAliases, but I didn't get a 'round tuit.' Let's do that
now.
--}

parseAndAddAliases :: FilePath -> AllianceMap -> IO AllianceMap
parseAndAddAliases = undefined

{--
Great, but now rerun the missing alliances-scenario again.

You get the same results (please verify this). Why?

Because, even though we now have aliases integrated into our AllianceMap, we
didn't consider aliases when doing the diff on what we have and what we don't.
Let's do that now.
--}

missingAlliancesWithAliases :: AllianceMap -> AllianceMap -> Set Name
missingAlliancesWithAliases = undefined

{--
Okay, great!

Now let's find a way to parse in these missing alliances, either with a parser
on the raw data, or with some hand-tuned modifications.
--}

incorporateMissingAlliances :: FilePath -> Set Name -> AllianceMap -> IO AllianceMap
incorporateMissingAlliances file missingAllianceNames alliances = undefined

{--
How many alliances do we have now? Also, check for empties. Do we have any
empty (that is: no country) alliances?
--}

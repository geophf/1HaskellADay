{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D23.Solution where

import Y2020.M10.D12.Solution    -- for AirBase
import Y2020.M10.D14.Solution (countriesByContinent, cbc)
import Y2020.M10.D15.Solution (countryMap)
import Y2020.M10.D20.Solution    -- for pared-down AirBase set and myUpdate

{--
So, I managed to update country-names in the graph yesterday, and then I
went to upload the air bases, but got the following error [snippet]:

\"errors\":[{\"code\":\"Neo.ClientError.Statement.SyntaxError\",
\"message\":\"Invalid input '2': expected '\\\\', ''', '\\\"', 'b', 'f', 'n', 
 'r', 't', UTF16 or UTF32 (line 1, column 73 (offset: 72))\\n\\
 \"MERGE (a:Base { url: \\\"http://www.wikidata.org/entity/Q43363\\\",
  name: \\\"Chi\\\\232vres Air Base\\\",icao: \\\"EBCV\\\"

This (hot mess) tells me that wikidate is doing some encoding into its JSON
output, changing unicode characters to \232, for example (e-grave, in this
case).

So, one more step, then: let's find all cases of base-names that have
escaped unicode, and return those bases.

THEN: Let's replace the escaped values with unicode. That shouldn't be
hard, right? It is the 21st century, after all, right?

Actually, ... looking at the raw data, this solution may be easier than
I anticipated. ... But no, changing the String-types to Text-types changes
the results by not one iota. Oh, well, it was a long detour that was worth
a try.

So, again, I say unto you, your new task:

Find hard coded \digits and replace with:

ABSOLUTELY NOTHING!

Yay.
--}

import Control.Arrow ((&&&), (>>>))

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (listToMaybe)

import qualified Data.Text as T    -- hint

import Graph.Query                 -- for graphEndpoint

stripNonAscii :: Map Icao AirBase -> Map Icao AirBase
stripNonAscii = foldr (myUpdate updater) <*> Map.keys

updater :: AirBase -> Maybe AirBase
updater ab@(Base _ e _ _ _) = 
-- updater' e >>= \e' -> return (ab { val = e' })
   case (unicodePoints e) of
      []       -> Nothing
      ts@(_:_) -> Just (ab { val = T.concat ts })

{--
updater' :: Entity -> Maybe Entity
updater' c = listToMaybe (
--                         (head &&& map (T.drop 3) . tail >>> uncurry (:))
                         (unicodePoints c))
--}

type Index = Int

unicodePoints :: Entity -> [Entity]
unicodePoints = T.split ((> 127) . ord)

-- How many AirBases were updated? What did you do for AirBases with multiple
-- unicode points in their entity-name

{--
>>> loadBases (Y2020.M10.D12.Solution.workingDir ++ file)
>>> let bs = it
>>> let mbs = byICAO bs

>>> countriesByContinent (Y2020.M10.D14.Solution.workingDir ++ cbc)
>>> let conti = it
>>> let cm = countryMap conti

>>> let newabm = firstPass mbs
>>> let nonu = stripNonAscii newabm
>>> Map.size nonu
921

>>> map val . filter ((== T.pack "Belgium") . country) $ Map.elems nonu
["Beauvechain Air Base","Kleine Brogel Air Base","Jehonville Air Base",
 "Chivres Air Base","Koksijde Air Base","Florennes Air Base","Bierset Airbase",
 "Melsbroek Air Base","Moorsele Airfield","Zutendaal Air Base",
 "Sint-Truiden Air Base","Saint-Hubert Air Base","Ursel Airbase",
 "Weelde Air Base"]

>>> graphEndpoint 
>>> let url = it
>>> uploadAirbases url nonu

yup. that worked.
--}

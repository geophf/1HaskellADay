module Y2018.M06.D29.Solution where

{--
We WERE going to do a graphing problem off of yesterday's LSI data stored in
JSON, but our PhD owned up to the problem and changed the format from a graph
that had to be parsed from a String -> String mapping to one that is now a
graph that has to be parsed from a String -> (String -> Double) mapping.

Great! Improvements!

However, he also refined his algorithm so that the LSI went from 10 megabytes
to 60 megabytes. I'm not going to put a 60 megabyte file up onto git for this
exercise, and, parsing in 60 megabytes? Can Haskell handle that, no problems?

Sure! Sure!

But let's say I don't wanna, and, instead, I want to create a row-parser
that streams in the data, as needed.

How do we do that?

Let's do that.

I have a sample wall-of-text, that, like the 60 megabyte file is all on one
line, but, unlike the 60 megabyte file, which is valid JSON, this included
file is truncated to 'one screen full,' so the sample provided here is
INVALID JSON. So you just can't Aeson this file in with the JSON parser and
declare victory, you actually have to create your own delimiter-parser, just
like we did in the good, old days with FORTRAN and punch-cards.

Here we go.
--}

import Control.Arrow (second)

import Data.Map (Map)
import qualified Data.Map as Map

exDir, lsiSlice :: FilePath
exDir = "Y2018/M06/D29/"
lsiSlice = "lsi_slice.txt"

-- lsi means "Latent semantic indexing," by the way.

type Idx = String
type Strength = Double
type IdxStrength = (Idx, Strength)

data LSI = LSI { idx :: Idx, strengths :: [IdxStrength] }
   deriving (Eq, Show)

{--
Okay, we don't have JSON but it is part of a JSON, ... our task today:

From this incomplete file, parse out the index and the index-strength-pairs

How do we do that? The partial file follows a regular grammar of JSON up to a
point so we know that

{ (very first) index : { index : strength , index : strength , ... }, ... }

And that's all we care about. Let's do that.
--}

parseByPair :: Read a => Read b => String -> [((a,b), String)]
parseByPair str =

-- we assume the cursor is at the start of the pair

   readsPrec 1 str >>= \(a,rest) ->
   readsPrec 1 (seekPast ':' rest) >>= \(b,restrest) ->
   return ((a,b),restrest)

seekPast :: Char -> String -> String
seekPast _ [] = []
seekPast c (h:t) = if h == c then trim t else seekPast c t

trim :: String -> String
trim [] = []
trim lst@(h:t) = if h == ' ' then trim t else lst

-- takes a string, parsed out the next pair and returns the rest of the string
-- with the next index on the needle

-- Those of you who never had a record player will be wondering about "needle."

-- What is the pair that you get?

{--
>>> (parseByPair "\"Foo\": \"Bar\"") :: [((String,String),String)] 
[(("Foo","Bar"),"")]
>>> (parseByPair "\"Foo\": 0.999821") :: [((String,Double),String)] 
[(("Foo",0.999821),"")]
--}

-- Now, convert that pair into an LSI value

pair2LSI :: (String, [IdxStrength]) -> LSI
pair2LSI = uncurry LSI

-- which may mean a map parser may be useful ... is parseByPair already a
-- map-parser? No. Can it be used to build such a function? Let's see.

mapParser :: Ord a => Read a => Read b => String -> [(Map a b, String)]
mapParser ('{':str) = parseByPair str >>= \(pair1,rest) ->
   mapParser' [pair1] rest
 
mapParser' :: Ord a => Read a => Read b => [(a,b)] -> String -> [(Map a b,String)]
mapParser' accum (',':str) =
   parseByPair (trim str) >>= \(pair,rest) ->
   mapParser' (pair:accum) rest
mapParser' accum ('}':rest) = return (Map.fromList accum,rest)

-- takes a string and parses out a { } map from it.

{--
>>> (mapParser "{ 5: 7, 2: 3}") :: [(Map Int Int, String)]
[(fromList [(2,3),(5,7)],"")]
--}

-- Is Parseable the same thing as ReadS? Should it be? Yes.

-- So, the final test:

{--
>>> (mapParser indices) :: [(Map String Double,String)]
[(fromList [("0",1.0),("10266",0.4510444700717926),
     ("103211",0.4893803596496582),("10366",0.4505229890346527),
     ("121638",0.4658055007457733),("12254",0.5607796311378479),
     ("12501",0.704148530960083),("12809",0.609732985496521), ...],"\n"]

... not exactly what I asked in the problem statement, but eh.
--}

module Y2018.M06.D29.Exercise where

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

import Data.Map (Map)

exDir, lsiSlice :: FilePath
exDir = "Y2018/M06/D28/"
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

parseByPair :: Parseable a => Parseable b => String -> ((a,b), String)
parseByPair str = undefined

-- takes a string, parsed out the next pair and returns the rest of the string
-- with the next index on the needle

-- Those of you who never had a record player will be wondering about "needle."

-- What is the pair that you get?

-- Now, convert that pair into an LSI value

pair2LSI :: (String, [IdxStrength]) -> LSI
pair2LSI pair = undefined

-- which may mean a map parser may be useful ... is parseByPair already a
-- map-parser? No. Can it be used to build such a function? Let's see.

mapParser :: Parseable a => Parseable b => String -> (Map a b, String)
mapParser str = undefined

-- takes a string and parses out a { } map from it.

-- this also may mean that you want a and b to be Parseable ... whatever that
-- means

class Parseable p where
   parse :: String -> (p, String)

-- Is Parseable the same thing as ReadS? Should it be?

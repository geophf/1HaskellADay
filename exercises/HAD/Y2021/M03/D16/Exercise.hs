module Y2021.M03.D16.Exercise where

{--
Yesterday we did some simple improvements to help unclutter our word-cloud.

Today, we're going to look at stemming words to see how that can help.

Also, we had too many ticks in our word-cloud (') ... ticks at the beginning
and ends of words are not apostrophes: they are punctuation and those ticks
should be eliminated (as ticks, in general, should be. Eheh.)

First, let's de-tick our cleaned-document.
--}

import Data.Map (Map)

import Y2021.M03.D15.Solution

detick :: Bag String -> Bag String
detick = undefined

{-- 
remember: handle key-collisions!

Now, let's use a natural language processor to stem our words:

The stemmer should output the token with the stemmed-representation

token,stem
--}

type Stem = String

readStems :: FilePath -> Bag String -> IO (Map Stem (Bag String))
readStems = undefined

{--
Do you see what's going on here? The stems can point to more than one token,
so, for example: the stem 'comput' can point to the tokens 'computer' and
'computes' and 'computers' and 'computing'; each token with their own
word-counts.

Now, re-realize the stemmed words as unified, and counted tokens:
--}

rerealize :: Map Stem (Bag String) -> Bag String
rerealize = undefined

-- do this however you like. You could pick the first token for each stem
-- then sum the counts of the tokens for that stem/first-token.

-- rewrite the file out as a flattened list of words

rewrite :: FilePath -> Bag String -> IO ()
rewrite = undefined

-- note that cleanedDoc and rewrite have the same signature, but do different
-- things.

-- What does the new word-cloud look like now?

{-- BONUS! -------------------------------------------------------

1.

Are there any stems that are ... 'kinda' similar? Meaning, are there different
stems for the same word-root? Like 'program' and 'programm'? If so, find a way
(maybe double-metaphones?) to combine these similar stems, then regenerate
the word-cloud from that.

2. 

Too much data. Too much data is the problem, folks. Throwing a whole bunch of
inconsequential words doesn't help the reader. Find a way to eliminate words
that are less meaningful. Regenerate the word-cloud.
--}

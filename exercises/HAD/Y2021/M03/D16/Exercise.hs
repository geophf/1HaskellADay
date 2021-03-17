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

import Data.Set (Set)

import Y2021.M03.D15.Solution

detick :: Bag String -> Bag String
detick = undefined

{-- 
remember: handle key-collisions!

>>> wb <- wordBagFrom (graceHopperDir ++ "/cleaned.txt")
>>> length wb
848
>>> let ntic = detick wb
>>> length ntic
846

Okay, we got rid of 2 redundancies. Good.

>>> take 5 . sortOn (Down . snd) $ Map.toList ntic
[("hopper",40),("navy",24),("language",17),("computer",15),("programming",15)]

And, with a cleaner look, too.

Now, let's use a natural language processor to stem our words:

>>> cleanedDoc "Y2021/M03/D16/de-tick.txt" ntic

The stemmer should output the token with the stemmed-representation

token,stem

Using a little program I wrote (stemmer.py in this directory):

$ cd Y2021/M03/D16
$ python stemmer.py de-ticked.txt > stemmed.csv

We do see some common stems:

...
computation,comput
computer,comput
computerrelated,computerrel
computers,comput
computing,comput
...

Let's exploit that commonality.
--}

type Stem = String
type Stems = Map Stem (Set (String, Int))

readStems :: FilePath -> Bag String -> IO Stems
readStems = undefined

rerealize :: Stems -> Bag String
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

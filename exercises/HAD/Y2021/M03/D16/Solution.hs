{-# LANGUAGE TupleSections #-}

module Y2021.M03.D16.Solution where

{--
Yesterday we did some simple improvements to help unclutter our word-cloud.

Today, we're going to look at stemming words to see how that can help.

Also, we had too many ticks in our word-cloud (') ... ticks at the beginning
and ends of words are not apostrophes: they are punctuation and those ticks
should be eliminated (as ticks, in general, should be. Eheh.)

First, let's de-tick our cleaned-document.
--}

import Control.Arrow (first, (&&&))

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Ord  -- for Down

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Map (snarf)
import Control.Scan.CSV (csv)

import Y2021.M03.D15.Solution

detick :: Bag String -> Bag String
detick = mergeBag . map (first (dt' . reverse . dt' . reverse)) . Map.toList

-- ... MWA-HAHA!

dt' :: String -> String
dt' [] = []
dt' l@(h:t) | h == '\'' = dt' t
            | otherwise = l

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
readStems stemmies baggies =
   snarf (slurp baggies . csv) . tail . lines <$> readFile stemmies

slurp :: Bag String -> [String] -> Maybe (Stem, (String, Int))
slurp baggie [a,b] = (b,) . (a,) <$> Map.lookup a baggie
slurp _ _ = Nothing

{--
>>> rstems <- readStems "Y2021/M03/D16/stemmed.csv" ntic
>>> length rstems
752

... interesting! Smaller set of words with stemming.

>>> head $ Map.toList rstems
("1",fromList [("1",2)])
--}

rerealize :: Stems -> Bag String
rerealize = Map.fromList . mapMaybe rr' . Map.elems

rr' :: Set (String, Int) -> Maybe (String, Int)
rr' s = (,sum $ Set.map snd s) . fst . fst <$> Set.minView s

-- do this however you like. You could pick the first token for each stem
-- then sum the counts of the tokens for that stem/first-token.

{--
>>> let rreals = rerealize rstems 
>>> take 5 . sortOn (Down . snd) $ Map.toList rreals 
[("hopper",40),("computation",28),("language",25),("navy",24),("program",18)]

... and a bit of reordering of words, taking into account common stems.
--}

-- rewrite the file out as a flattened list of words

rewrite :: FilePath -> Bag String -> IO ()
rewrite = cleanedDoc

-- note that cleanedDoc and rewrite have the same signature, but do different
-- things. OR. DO. THEY?

{--
>>> rewrite "Y2021/M03/D16/rerealized.txt" rreals 
--}

-- What does the new word-cloud look like now?

{-- BONUS! -------------------------------------------------------

1.

Are there any stems that are ... 'kinda' similar? Meaning, are there different
stems for the same word-root? Like 'program' and 'programm'? If so, find a way
(maybe double-metaphones?) to combine these similar stems, then regenerate
the word-cloud from that.

... nah. I didn't see pervasive kinda-similars.

2. 

Too much data. Too much data is the problem, folks. Throwing a whole bunch of
inconsequential words doesn't help the reader. Find a way to eliminate words
that are less meaningful. Regenerate the word-cloud.

>>> let nones = Map.filter (> 1) rreals 
>>> length nones
226

>>> rewrite "Y2021/M03/D16/nones.txt" nones

That filtered out a lot of words, ... but I like the below representation better:

>>> let ntwos = Map.filter (> 2) rreals 
>>> length ntwos
117

>>> rewrite "Y2021/M03/D16/ntwos.txt" ntwos

... and this one (below): the most

>>> let nthrees = Map.filter (> 3) rreals 
>>> length nthrees
71

>>> rewrite "Y2021/M03/D16/nthrees.txt" nthrees

... so: ... I guess the 'magic-sauce' is under 100 words for a comprehensible
word-cloud, then? Works for me!
--}

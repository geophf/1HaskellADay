{-# LANGUAGE ViewPatterns #-}

module Y2017.M11.D13.Solution where

-- Okay, we've got keywords from

import Y2017.M11.D03.Solution

{--
Now, let's use them. Today we're going to scan into our keyword set and find
articles that contain these keywords.

There's a catch: these keywords are actually key-phrases as well as keywords. 
What if a key-phrase has the sought word? Are we going to rend the key-phrase 
into words and scan that? Are we going to front-load, then memoize, that work?

Let's see.

Using the work from before (kwDir, readKeyphrases), get the keywords from 
Y2017/M11/D03/

Okay, now create a search-algorithm. It takes a set of keys as and-constraints, 
and returns the set of article ids matching those keyword constraints.

But before you do that, you probably want to do an inversion: the 
KeyphraseMap-type is of the form 
--}

import Control.Arrow (second)
import Data.Ord
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type KeyWord = String -- denotes a sigle word
type ArticleId = Integer

type IndexedArticles = Map KeyWord (Set ArticleId)

searchKeymap :: KeyphraseMap -> IndexedArticles
searchKeymap = foldr addKWs Map.empty . Map.assocs
      where addKWs (art, invert -> kws) = flip (addKeyWords art) kws

{-- 
The above function is the goal, but before we do that, we want to break up the 
key-phrases in the keyword map into individual keywords, and then map the 
article Id for those keywords. Is there going to be redundancy? Yes, but then, 
that's why there're sets on both ends of the mapping: the map's keys are a set, 
and each map key points to a set. Let's see how this goes.

So, for each entry in the keyword map, we want to extract the words of the 
Keyphrase values then add all those words the article Id to their respective 
sets.
--}

invert :: [Keyphrase] -> Set KeyWord
invert = Set.fromList . concatMap (words . string . keyphrase)

-- Now that we have the words we add the article Id to the sets in the map.
-- If there is no entry for key word, we initialize it with a singleton of 
-- article id

addKeyWords :: ArticleId -> IndexedArticles -> Set KeyWord -> IndexedArticles
addKeyWords artid arts =
   foldr (flip (Map.insertWith Set.union) (Set.singleton artid)) arts
       . Set.toList

{--
Do that iteratively over the entire keyword set and you will have your indexed 
articles.

>>> kws <- readCompressedKeyphrases (kwDir ++ "refinedEr_kws.txt.gz")
>>> kwmap = searchKeymap kws

Now. How many unique KeyWords do you have?

>>> length kwmap
28586

... which is ~ 10x fewer keywords than Keyphrase entries.

What keyword has the most articles? What keyword has the second most number of 
articles?

>>> let sordid = sortOn (Down . length . snd) (Map.assocs kwmap)
>>> take 5 (map (second length) sordid)
[("time",2292),("people",2162),("united",1443),("trump",1082),("week",1022)]

What is the intersection of those two keywords?

Put another way: given a list of keywords, find the articles that intersect all 
those keywords.
--}

refineSearch :: IndexedArticles -> [KeyWord] -> Set ArticleId
refineSearch arts = intersects .  map (emptyOr . (`Map.lookup` arts))

emptyOr :: Maybe (Set a) -> Set a
emptyOr Nothing = Set.empty
emptyOr (Just s) = s

intersects :: Ord a => [Set a] -> Set a
intersects [] = Set.empty
intersects (h:t) = foldr Set.intersection h t

{--
>>> length $ refineSearch kwmap (words "time people")
778
--}

-- We'll look tomorrow at incorporating the metadata of the articles (that we 
-- get from the article Ids), such as publish_dt, to narrow the search further.

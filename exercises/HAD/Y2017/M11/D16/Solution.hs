{-# LANGUAGE ViewPatterns #-}

module Y2017.M11.D16.Solution where

{--
Yesterday we used keywords, rended from key-phrases, and were able to narrow a
search through a database of articles. Great!

Well, some 'narrowed' searches returned 2000+ articles, or 778 articles. That's
still a lot of ground to cover, and the search results didn't rank the returned
results. How do we know what articles were most relevant? And how do we put
those articles first?

The problem with (my) yesterday's solution was that I got the keywords rendered
from the key-phrases, but I didn't use the key-phrase-strength associated with
the source article.

Today's Haskell problem is to associate those keywords back to their source
key-phrases so that the relevance of an article can be ranked, using the key-
phrase strength as the determinant.

That is to say:

1. for each keyword, k, provide a (multi-)mapping from k to its key-phrases.

Then:

2. for each recommended article, a, provide a strength that is the sum of its
   key-phrases associated with the keywords use to narrow the search to this
   article.

Then:

3. Return the ranked article set from a keyword-search, most relevant article
   first.
--}

-- 1. We have a KeyWord -> Set ArticleId mapping. Provide a 
--    KeyWord -> Set Keyphrase mapping

-- Oh, and KeyWord and Keyword look bloody well the same, why? Because
-- key-phrases are called keywords for 'hysterical raisins.' That is to say:
-- somewhere along the line, somebody started classifying articles by 
-- key-phrases instead of just by keywords and never updated the terminology.

import Control.Arrow ((&&&))
import Data.Ord
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Y2017.M11.D03.Solution  -- Keyphrase and its ilk
import Y2017.M11.D13.Solution  -- KeyWord and its ilk

type KeyWordKeyphrases = Map KeyWord KeyphraseMap

{--

Okay, hold the phone. Maybe computing all this for each search is overdoing it
But maybe we can upload this into the data store then retrieve the relevant
key-phrases and their strengths on a SQL query.

Hm.

So, okay, we'll do all this crunch-work and then upload the results to the
database for later retrieval on a single-keyword request (instead of redoing
it each time a request comes in).
--}

-- this is an n x m search. ugh. Here we go.

kw2kw :: KeyphraseMap -> IndexedArticles -> KeyWordKeyphrases
kw2kw kphrmap = foldr (\(kw, Set.toList -> arts) ->
           -- we know that each artid is in the kwmap
                Map.insert kw (foldr (\art ->
    -- so we get the relevant key-phrases for this kw
        Map.insert art (filter (has kw) (kphrmap Map.! art))) Map.empty arts))
                      Map.empty . Map.assocs
      where has kw = any (== kw) . words . string . keyphrase

-- maps the KeyWord values to sets of keyphrased and their associated articles
-- include ONLY the keyphrases associated with the keyword mapped.

{--
>>> kws <- readCompressedKeyphrases (kwDir ++ "refinedEr_kws.txt.gz")
>>> kwmap = searchKeymap kws

Now, with the results of yesterday:

1. what are the first five articles returned from yesterday's refined search?

>>> refined = refineSearch kwmap (words "time people")
>>> length refined
778

>>> take 5 (Set.toList refined)
[13,20,23,50,86]

Now, rank articles by associated strength, that is to say, accumulate and add
the strengths of the keyphrases for the articles for the keywords searched

>>> linked = kw2kw kws kwmap 
--}

ranked :: KeyWordKeyphrases -> [KeyWord] -> Set ArticleId -> [(ArticleId, Strength)]
ranked kwjoin (mapMaybe (`Map.lookup` kwjoin) -> keytables) =
   sortOn (Down . snd) . map (id &&& flip summer keytables) . Set.toList
      where summer art = sum . map strength . concat . mapMaybe (Map.lookup art)

-- 2. What, now are the top five (that is to say: the top five highest ranked)
--    articles?

{--
>>> take 5 (ranked linked (words "time people") refined)
[(4333,21.977348738218303),(319,17.2693388736867),(6020,16.85),
 (7750,16.81515103254234),(141,12.742719838778438)]

HELLA different. Mm, hm!
--}

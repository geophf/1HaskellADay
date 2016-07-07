module RID.Analysis where

{-- a solution to the problem posted at http://lpaste.net/2340251327956779008
So, 'yesterday' ...

Actually, yesterday, I did a presentation of a graph database to our internal
team, gonna present that to the customer on Monday. Fun stuff.

So, 'yesterday' we parsed a small document (a 1-liner) with a small, 1-category,
RID. http://lpaste.net/237534433320632320

TODAY, we'll parse a larger document with the full RID.

Let's do this.

But what is 'this' that we're gonna 'do'?

From a RID, parse a document into its RID-paths and then report out how much
of the document (percentage-wise) was PRIMARY, how much was SECONDARY, and
how much was EMOTIONS.
--}

import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.Monoid

import Data.Bag
import Data.Percentage

import RID.Graph
import RID.Parser
import RID.Tree

data RIDAnalysis = RA { breakdown :: Bag Path, primaries :: Percentage,
                        secondaries :: Percentage, emotions :: Percentage }
   deriving Show

parseDocument :: RID -> String -> RIDAnalysis
parseDocument rid = regressiveAnalysis . matcher rid

-- All the accumulated RID-resources are available to you here:
-- http://logicaltypes.blogspot.com/p/haskell-libraries.html
-- (bottom of page)

-- So, go to Project Gutenberg: http://www.gutenberg.org
-- Pick a book you like in plain-text format and run it through the RID.
-- What are your results?

{-- So, let's do the most popular: Pride and Prejudice, or 1342.txt
*Main> readinRID "RID" ~> rid
*Main> urlToTxt "http://www.gutenberg.org/cache/epub/1342/pg1342.txt" ~> pnp
*RID.Parser> matcher rid pnp ~> ... after a LONG time ... :/

... we need to look at efficiencies here ...

so:

*RID.Parser> take 10 . reverse $ Bag.rank bag ~>
[(["PRIMARY","REGR_KNOL","CONCRETENESS","AT (1)"],Sum {getSum = 800}),
 (["SECONDARY","ABSTRACT_TOUGHT","THEM* (1)"],Sum {getSum = 469}),
 (["SECONDARY","TEMPORAL_REPERE","EVER* (1)"],Sum {getSum = 418}),
 (["SECONDARY","SOCIAL_BEHAVIOR","SAID* (1)"],Sum {getSum = 401}),
 (["SECONDARY","TEMPORAL_REPERE","WHEN* (1)"],Sum {getSum = 397}),
 (["SECONDARY","ABSTRACT_TOUGHT","KNOW* (1)"],Sum {getSum = 352}),
 (["SECONDARY","RESTRAINT","MUST* (1)"],Sum {getSum = 318}),
 (["SECONDARY","MORAL_IMPERATIVE","SHOULD* (1)"],Sum {getSum = 251}),
 (["PRIMARY","SENSATION","SOUND","HEAR* (1)"],Sum {getSum = 244}),
 (["SECONDARY","ABSTRACT_TOUGHT","THINK* (1)"],Sum {getSum = 236})]
--}

wordcount :: [(a, Sum Int)] -> Sum Int
wordcount = sum . map snd

divy :: [(Path, Sum Int)] -> [Sum Int]
divy = map (sum . map snd) . grouping

grouping :: [(Path, Sum Int)] -> [[(Path, Sum Int)]]
grouping = groupBy (curry (uncurry (==) . join (***) (head . fst))) . sort

{--
*Main> divy $ Map.toList bag ~>
[Sum {getSum = 4194},Sum {getSum = 7058},Sum {getSum = 14379}]

... yes, but what are these groupings?

*Main> let grp = grouping $ Map.toList bag
*Main> head (head grp) ~>
(["EMOTIONS","AFFECTION","AFFECT* (1)"],Sum {getSum = 109})
*Main> head (head (tail grp)) ~>
(["PRIMARY","DEFENSIVE_SYMBOL","CHAOS","CHANC* (1)"],Sum {getSum = 20})
*Main> head (head (tail (tail grp))) ~>
(["SECONDARY","ABSTRACT_TOUGHT","ALMOST* (1)"],Sum {getSum = 61})

Sorted alphabetically, of course!
--}

regressiveAnalysis :: Bag Path -> RIDAnalysis
regressiveAnalysis bag =
   let nada    = Map.fromList
          (zip [["EMOTIONS"], ["PRIMARY"], ["SECONDARY"]] (repeat (Sum 0)))
       list    = Map.toList (bag <> nada)
       wc      = toRat (wordcount list)
       [e,p,s] = map (P . flip (/) wc . (100 *) . toRat) (divy list)
   in  RA bag p s e

toRat :: Sum Int -> Rational
toRat = toRational . getSum

{-- So:
*Main> let (RA bag p s e) = parseDocument rid pnp
*Main> (p, s, e) ~> (27.53%,56.10%,16.36%)
--}

{-- BONUS -------------------------------------------------------------------

Do this for several documents. Do you notice any patterns?

Thought for next week: 'You may also like...'

Based on the documents you've selected, can these documents be classified along
preferential lines?
--}

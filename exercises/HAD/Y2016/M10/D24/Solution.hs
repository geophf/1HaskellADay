module Y2016.M10.D24.Solution where

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658

-- below modules available from 1HaskellADay git repository

import Graph.KMeans
import Graph.ScoreCard

import Y2016.M10.D19.Solution (LeadersLosers)
import Y2016.M10.D21.Solution (top5s2ScoreCards)

{--
So, 'yesterday' (Friday) you were able to take a set of daily stock reports

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

and from that report give a set of score cards, one per stock, on their top5s
showings.

Great.

So now you have score cards, which are the feed to a little k-means algorithm
we developed back in March 2016.

So, today: cluster the score cards using the k-means algorithm.
--}

type NClusters = Int

type Top5sScore a = ScoreCard Symbol LeadersLosers a

kmeansMeBaybee :: Num a => NClusters -> [Top5sScore a]
               -> (Int, SCClusters Symbol LeadersLosers a)
kmeansMeBaybee = undefined

-- hint: we get our scorecards from top5s2ScoreCards from imports above

-- Question: how many generations did it take to arrive a this number of clusters?

-- Questions: what number of clusters is 'optimal,' according to you? 5? 10? more?

{-- BONUS -----------------------------------------------------------------

There is such a thing as scale. If a particular attribute, say, volume, far
out-shows all the others, it then may become dominant, overshadowing possibly
significant differences in other attributes.

So, for the bonus, scale all the attributes on the continuum (or near enough
for floats) from 0.0 - 1.0

--}

scale :: [Top5sScore Int] -> [Top5sScore Float]
scale = undefined

-- hint: scale may (or may not) be comonadic.

-- now, with the score cards rescaled: what are the new results from the
-- k-means clustering? Do the new clusters make more or less sense to you?

{-- BONUS-BONUS -----------------------------------------------------------

Graph the clustered results, giving a visual representation of your clusters.

Use whatever drawing/graphing tool you like.

--}

grapheme :: SCClusters a b c -> IO ()
grapheme clusters = undefined

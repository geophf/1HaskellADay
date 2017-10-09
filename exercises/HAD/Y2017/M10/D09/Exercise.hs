{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D09.Exercise where

{--
So, 'yesterday' we saved our data out as JSON, read it back in, and then
visualized it. The data was the NYT article archive, and, doing that, we
saw we had a lot of data to visualize.

Today, let's par that down a smidge.
--}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Set (Set)

-- below imports available via 1HaskellADay git repository

import Graph.D3.CirclePacking -- hint: use this to visualize your results

import Y2017.M10.D04.Exercise
import Y2017.M10.D05.Exercise

-- From the grouping that you formed before (so, you have to reformulate that
-- grouping again, today), par it down to the top 5 topics, that is to say:
-- the topics that have the most articles

topicality :: Grouping -> [(Topic, Int)]
topicality groups = undefined

-- topicality gives us the number of articles of that topic

-- Now that you have topicality, reform the group to contain only those
-- top 5 topics (and no other topics)

reformGrouping :: Set Topic -> Grouping -> Grouping
reformGrouping hotTopics groups = undefined

-- Great! Now let's visualize that subset. What do you get? Tweet your results

{-- BONUS -----------------------------------------------------------------

* What are the topics of the NYT archive slice that have 10 or more articles?
* How many topic make the cut of ten or more articles?
* Chart those topics
--}

top10sTopics :: Grouping -> [(Topic, Int)]
top10sTopics grps = undefined

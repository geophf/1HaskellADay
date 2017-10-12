{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D09.Solution where

{--
So, 'yesterday' we saved our data out as JSON, read it back in, and then
visualized it. The data was the NYT article archive, and, doing that, we
saw we had a lot of data to visualize.

Today, let's par that down a smidge.
--}

import Control.Arrow (second)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Data.Hierarchy          -- hint: use this to visualize your results
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Solution
import Y2017.M10.D05.Solution

-- From the grouping that you formed before (so, you have to reformulate that
-- grouping again, today), par it down to the top 5 topics, that is to say:
-- the topics that have the most articles

topicality :: Grouping -> [(Topic, Int)]
topicality = map topicCount . Map.toList

-- topicality gives us the number of articles of that topic

topicCount :: (a, Map b [c]) -> (a, Int)
topicCount = second (length . concat . Map.elems)

{--
>>> subjs <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/subj.json") :: IO [Subject]
>>> pivs <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/art-subj.json") :: IO [Pivot]
>>> arts <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/art.json") :: IO [ArticleSummary]
>>> let grp = graphTopics subjs pivs arts 
>>> let tops = topicality grp
>>> let top5 = take 5 (sortOn (Down . snd) tops)
>>> top5
[("Social networks",42),("Presidents",30),("Hurricanes",25),
 ("Books",19),("Floods",16)]
--}

-- Now that you have topicality, reform the group to contain only those
-- top 5 topics (and no other topics)
 
reformGrouping :: Set Topic -> Grouping -> Grouping
reformGrouping tops =
   Map.fromList . filter (flip Set.member tops . fst) . Map.toList

{--
>>> let subgrps = reformGrouping (Set.fromList (map fst top5)) grp
>>> length subgrps 
5
--}

-- Great! Now let's visualize that subset. What do you get? Tweet your results

{--
>>> visualize "Y2017/M10/D09/topics.json" subgrps

$ php -S 127.0.0.1:8080 &

... and we see the concentric circle representation Y2017/M10/D09/top5-topics.png
--}

{-- BONUS -----------------------------------------------------------------

* What are the topics of the NYT archive slice that have 10 or more articles?
* How many topic make the cut of ten or more articles?
* Chart those topics
--}

top10sTopics :: Grouping -> [(Topic, Int)]
top10sTopics = takeWhile ((> 9) . snd) . sortOn (Down . snd) . topicality

{--
>>> let top10s = top10sTopics grp
>>> length top10s
21
>>> top10s
[("Social networks",42),("Presidents",30),("Hurricanes",25),("Books",19),
 ("Floods",16),("Aliens",15),("Immigration policy",15),("Motion pictures",15),
 ("Theater",14),("White supremacists",14),("Bills",13),("Storm damage",13),
 ("Students",13),("Families & family life",12),("Tournaments & championships",12),
 ("Deportation",11),("Politics",11),("Tennis",11),("Art galleries & museums",10),
 ("Criminal investigations",10),("Political parties",10)]
>>> let subgrps = reformGrouping (Set.fromList (map fst top10s)) grp
>>> visualize "Y2017/M10/D09/top10-topics.json" subgrps

And now we see th new presentation. NOICE!
--}

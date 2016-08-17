{-# LANGUAGE TupleSections #-}

module Y2016.M08.D17.Solution where

import Control.Arrow ((>>>), (&&&))
import Control.Monad (mplus)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- the below imported modules are available on this github repository

import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Twitter
import Graph.JSON.Cypher.Read.Graphs
import Graph.JSON.Cypher.Read.Tweets

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
"relationships":[{"id":"1447",
                  "type":"USING",
                  "startNode":"255","endNode":"1000",
                  "properties":{}}]}}
--}

-- first up we want a set of indexed tweets from our graph-JSON
-- recall readGraphJSON twitterGraphUrl gives us the graph-JSON

-- note: I've generalized the Tweet-type in Data.Tweet

uniqueTweets :: [GraphJ] -> Set (Tweet String)
uniqueTweets = 
   indexedTweets           >>>
   Map.map t2tt            >>>
   Map.toList              >>>
   map (uncurry IndexedT)  >>>
   Set.fromList

{--
*Y2016.M08.D17.Solution> readGraphJSON twitterGraphUrl ~> tweets
*Y2016.M08.D17.Solution> let unqt = uniqueTweets tweets ~> length ~> 29
*Y2016.M08.D17.Solution> head (Set.toList unqt) ~>
IndexedT {index = "1134", tt = TT {date = 2016-05-20, ...}}
--}

data Direction = GoingTo | ComingFrom
   deriving (Eq, Ord)

instance Show Direction where
   show GoingTo = ">"
   show ComingFrom = "<"

data Dart a = Drt Direction Label a
   deriving (Eq, Ord)

instance Show a => Show (Dart a) where
   show (Drt GoingTo lbl val) = ' ':lbl ++ "> " ++ show val
   show (Drt ComingFrom lbl val) = " <" ++ lbl ++ " " ++ show val

relatedData :: Set (Tweet String) -> [GraphJ]
            -> MultiMap (Tweet String) (Dart String) (Set (Dart String))
relatedData tweets =
   let mt = Map.fromList (map (index &&& id) (Set.toList tweets)) in
   concatMap rels                                                   >>>
   mapMaybe (\(RJ _ knd st en _) ->
               fmap (, Drt GoingTo knd en) (Map.lookup st mt)
       `mplus` fmap (, Drt ComingFrom knd st) (Map.lookup en mt))   >>>
   MM.fromList Set.singleton

-- Okay, this function is 'a little' thick. It says:
-- for each tweet
-- from all the relationships
-- add all data coming from this tweet, and
-- add all data going to this tweet
-- ... as a multi-map.

{--
*Y2016.M08.D17.Solution> let reld = relatedData unqt tweets ~> head . MM.toList ~>
(IndexedT {index = "1134", tt = TT {date = 2016-05-20,...}},
 fromList [CONTAINS> "1494", REPLY_TO> "1241", TAGS> "1011", USING> "1400"])
--}

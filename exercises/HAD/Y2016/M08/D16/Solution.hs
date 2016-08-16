{-# LANGUAGE OverloadedStrings #-}

module Y2016.M08.D16.Solution where

{--
Now that we have graph information, let's start resolving these NodeJ-values
into more specific types.

From the import above, read in the JSON-as-graphs and from the nodes of that
graph return all the tweets-as-TimedTweets.
--}

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network.HTTP.Conduit (simpleHttp)

import Graph.JSON.Cypher.Read.Graphs
import Y2016.M08.D09.Solution hiding (tweet)
import Y2016.M08.D10.Solution hiding (indexedTweets)
import Y2016.M08.D15.Solution (twitterGraphUrl)

tweetFrom :: PropertiesJ -> Tweet
tweetFrom (PJ props) = 
   fromJust (Tweet <$> props <<-$ "id_str" <*> props <<-$ "text"
                   <*> props <<-$ "created_at" <*> props <<-# "favorites")

-- Some helper functions to extract Value-values and unbox while extracting

(<<-$) :: Ord a => Map a Value -> a -> Maybe String
x <<-$ y = fmap unboxs (Map.lookup y x)

unboxs :: Value -> String
unboxs (String s) = T.unpack s

(<<-#) :: (Ord a, Integral b) => Map a Value -> a -> Maybe b
x <<-# y = fmap unboxn (Map.lookup y x)

unboxn :: Integral a => Value -> a
unboxn (Number n) = floor (read $ show n)
 
-- from the PropertiesJ, then convert that to a TimedTweet using t2tt.

{--
*Y2016.M08.D16.Solution> readGraphJSON twitterGraphUrl ~> tweets
*Y2016.M08.D16.Solution> let twit = last . nodes $ head tweets
*Y2016.M08.D16.Solution> twit ~> 
NJ {idn = "255",labels = ["Tweet"],propsn = PJ (fromList [("created_at",...)])}
*Y2016.M08.D16.Solution> tweetFrom (propsn twit) ~>
Tweet {idx = "727179491756396546", txt = "April 2016 @1HaskellADay...", ...}
--}

-- Using the above definition, define the below

-- first, we need to know which nodes are tweets:

isTweet :: NodeJ -> Bool
isTweet = elem "Tweet" . labels

-- And then we get all nodes from the graph that are tweets

filterTweetNodes :: [GraphJ] -> [NodeJ]
filterTweetNodes = filter isTweet . concatMap nodes

-- okay, for the indexed tweets, we now no longer need to determine an index
-- as the cypher query result generates one for its internal use and shares it
-- with us! Convenient.

indexedTweets :: [GraphJ] -> Map String Tweet
indexedTweets = 
   Map.fromList . map (idn &&& tweetFrom . propsn) . filterTweetNodes

{-- 
How many unique tweets are in the data set?
*Y2016.M08.D16.Solution> let idxt = indexedTweets tweets
*Y2016.M08.D16.Solution> length idxt ~> 33
--}

-- And with all the above we have:

tweetsFrom :: FilePath -> IO [TimedTweet]
tweetsFrom = fmap (map t2tt . Map.elems . indexedTweets) . readGraphJSON

{--
*Y2016.M08.D16.Solution> fmap head (tweetsFrom twitterGraphUrl) ~>
TT {date = 2016-05-20, time = 18:32:47, 
    twt = Tweet {idx = "733727186679672833", 
                 txt = "The weight's the thing\nWherein I'll catch the "
                    ++ "conscience of the King\n... no ... weight ...\nToday's "
                    ++ "#haskell solution https://t.co/XVqbPRfjAo",
                 created = "Fri May 20 18:32:47 +0000 2016",
                 favs = 1}}
--}

-- We'll look at the nodes related to the tweets (User, Link, Source)
-- throughout the rest of this week

-- adding the <<-$/# operators to the JSON read module, and am thinking I'll
-- need to start a twitter module, as well?

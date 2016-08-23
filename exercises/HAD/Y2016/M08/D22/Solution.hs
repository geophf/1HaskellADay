{-# LANGUAGE OverloadedStrings #-}

module Y2016.M08.D22.Solution where

{--
So, we've looked at tweets, urls/links, users, and so we have just a bit left
(that concerns me, anyway), and that is the Hashtag (or '#brown' to tweeps in
the know). The structure of a hashtag is buried somewhere in these data:

"graph":
 {"nodes":
  [{"id":"1011","labels":["Hashtag"],"properties":{"name":"haskell"}},
   {"id":"255","labels":["Tweet"],
    "properties":{"id":727179491756396500,
                  "id_str":"727179491756396546",
                  "text":"April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX",
                  "created_at":"Mon May 02 16:54:35 +0000 2016",
                  "favorites":0}}],
  "relationships":
  [{"id":"1549","type":"TAGS","startNode":"255","endNode":"1011","properties":{}}]}

So, yeah: the Hashtag

tease out the hashtags from the twitter graph-JSON at twitterGraphUrl
--}

import Control.Arrow ((&&&))
import Control.Monad (liftM2, mplus)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- from the git repository here:

import Data.Bag
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Twitter
import Graph.JSON.Cypher.Read.Graphs
import Graph.JSON.Cypher.Read.Tweets

import Y2016.M08.D15.Exercise (twitterGraphUrl)

data Hashtag = Tag { tag :: String } deriving (Eq, Ord)

instance Show Hashtag where show = ('#':) . tag

instance FromJSON Hashtag where
   parseJSON (Object o) = Tag <$> o .: "name"

-- find the unique set of Hashtags indexed by hashtag id:

type HashtagMap = Map String Hashtag

hashtags :: [GraphJ] -> HashtagMap
hashtags = nodeMapper "Hashtag"

hashtagMap :: FilePath -> IO HashtagMap
hashtagMap = fmap hashtags . readGraphJSON

{-- How many hashtags are there?
*Y2016.M08.D22.Solution> hashtagMap twitterGraphUrl ~> browns ~> length ~> 6
*Y2016.M08.D22.Solution> mapM_ print browns
Tag {tag = "haskell"}
Tag {tag = "1liner"}
Tag {tag = "trie"}
Tag {tag = "datatype"}
Tag {tag = "summerhaskell2016"}
Tag {tag = "protip"}
--}

{-- BONUS -----------------------------------------------------------------

A bit of a data analysis. Can you identify a User by the hashtags they use?

List the users in the data set and the hashtags they use by frequency.
--}

-- How do we go about this? We map the user to their tweets (already done in
-- Read.Tweets) then we map the tweets to their hashtags then count hashtags
-- by users (via the tweets)

-- So, mapping tweets to hashtags

type MMtht = MultiMap (Tweet String) Hashtag (Set Hashtag)

hashtagsInTweets :: [GraphJ] -> MMtht
hashtagsInTweets rows =
   let relates = concatMap rels rows
       tweets  = Map.fromList (map (index &&& id) . Set.toList $ uniqueTweets rows)
       hashes  = hashtags rows
   in  foldr (addRowR tweets hashes) (MM.MM Map.empty Set.singleton) relates

addRowR ::TweetMap -> HashtagMap -> RelJ -> MMtht -> MMtht
addRowR tweets hashes rel mm =
   fromMaybe mm (tweethashR tweets hashes rel >>= \(t,h) ->
                 return (MM.insert t h mm))

tweethashR :: TweetMap -> HashtagMap -> RelJ -> Maybe (Tweet String, Hashtag)
tweethashR tweets hashes (RJ _ _ start end _) =
   comma start end `mplus` comma end start
      where comma t   = liftM2 (,) (findtweet t) . findhash
            findtweet = flk tweets
            findhash  = flk hashes
            flk       = flip Map.lookup

{--
*Y2016.M08.D22.Solution> readGraphJSON twitterGraphUrl ~> rows 
*Y2016.M08.D22.Solution> let htit = hashtagsInTweets rows ~> length ~> 14
*Y2016.M08.D22.Solution> mapM_ print . Map.toList . Map.mapKeys index $ MM.store htit
("1134",fromList [#haskell])
("1222",fromList [#haskell])
("1238",fromList [#datatype,#haskell,#trie])
("1240",fromList [#haskell])
("1241",fromList [#haskell])
("1242",fromList [#haskell])
("1243",fromList [#haskell])
("1244",fromList [#summerhaskell2016])
("1246",fromList [#haskell,#protip])
("1247",fromList [#haskell])
("153",fromList [#haskell])
("255",fromList [#haskell])
("941",fromList [#1liner])
("942",fromList [#haskell])
--}

-- now that we have that, we look up tweet by user the return the bag of #tags

hashtagsPerUser :: MMtht -> MMt -> User -> Bag Hashtag
hashtagsPerUser browns uts usr = 
   foldr add emptyBag . concatMap Set.toList . map (`MM.lookup` browns)
       . Set.toList $ MM.lookup usr uts

-- and then we do the roll-up to get our solution:

hashtagByUser :: [GraphJ] -> Map User (Bag Hashtag)
hashtagByUser rows =
   let users = userTweets rows
       browns = hashtagsInTweets rows
   in  Map.fromList . map (id &&& hashtagsPerUser browns users)
     . Set.toList $ MM.keysSet users

{--
*Y2016.M08.D22.Solution> let htbu = hashtagByUser rows ~> length ~> 7
*Y2016.M08.D22.Solution> mapM_ print . Map.toList . Map.mapKeys name $ id htbu
("1HaskellADay",fromList [(#1liner,Sum {getSum = 1}),(#haskell,Sum {getSum = 3})])
("Aaron Levin",fromList [])
("Amar Potghan",fromList [])
("Edward Kmett",fromList [(#summerhaskell2016,Sum {getSum = 1})])
("Francisco  T",fromList [])
("Gabriel Gonzalez",fromList [(#haskell,Sum {getSum = 1}),(#protip,Sum {getSum = 1})])
("geophf \217\8224",fromList [(#haskell,Sum {getSum = 1})])

WOOT! There you have it: hashtags by user.
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2016.M08.D19.Solution where

import Control.Arrow ((&&&), (>>>))
import Control.Monad (liftM2, mplus)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Twitter
import Graph.JSON.Cypher.Read.Graphs
import Graph.JSON.Cypher.Read.Tweets

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
So, yesterday we extracted URLs from twitter graph-JSON.

Today we'll extract users, or, in twitter-parlance: tweeps.

From the twitter graph-data at twitterGraphUrl, extract the users. The user
node data has the following structure:

{"id":"504",
 "labels":["User"],
 "properties":{"screen_name":"1HaskellADay",
               "name":"1HaskellADay",
               "location":"U.S.A.",
               "followers":1911,
               "following":304,
               "profile_image_url":"http://pbs.twimg.com/profile_images/437994037543309312/zkQSpXnp_normal.jpeg"}

Reify these data into a Haskell-type
--}

data User = Tweep { screen, name, location :: String,
                    followers, following :: Int,
                    icon :: String }
   deriving (Eq, Ord, Show)

instance FromJSON User where
   parseJSON (Object o) =
      Tweep <$> o .: "screen_name" <*> o .: "name"
            <*> o .: "location"    <*> o .: "followers"
            <*> o .: "following"   <*> o .: "profile_image_url"

-- How many unique users are found in this data-set?

-- well, we get the user-nodes first:

userNodes :: [GraphJ] -> [NodeJ]
userNodes = filter ((elem "User") . labels) . concatMap nodes

{--
*Y2016.M08.D19.Solution> readGraphJSON twitterGraphUrl ~> rows ~> length ~> 100
*Y2016.M08.D19.Solution> let users = userNodes rows ~> length ~> 21

So we have 21 user nodes. How many are parsed into user values?
--}

-- We convert each user node to a user value:

tweep :: NodeJ -> Maybe (String, User)
tweep = idn &&& res2mb . fromJSON . prop2obj . propsn >>> sequence

-- *Y2016.M08.D19.Solution> let twitters = mapMaybe tweep users ~> length ~> 21

-- Great! All of them! Now let's find the unique users of the data-set.

uniqueUsers :: [GraphJ] -> Map String User
uniqueUsers = Map.fromList . mapMaybe tweep . userNodes

-- *Y2016.M08.D19.Solution> uniqueUsers rows ~> length ~> 7
-- fromList [("137",Tweep {screen = "1HaskellADay", ...}), ...]

-- And with that we define our read-function

readTwitterUsers :: FilePath -> IO (Map String User)
readTwitterUsers = fmap uniqueUsers . readGraphJSON

-- TA-DAH!

{-- BONUS -----------------------------------------------------------------

From Read.Tweets we can get [GraphJ] of the nodes and relations. Answer the
below:

What is the distribution of tweets to users?
--}

type MMt = MultiMap User (Tweet String) (Set (Tweet String))

{--
Set is not a monad???

instance Applicative Set where
   pure x = Set.singleton x

instance Monad Set where
   return x = Set.singleton x
   join m   = Set.unions (Set.toList m)

join is not minimally-complete for monad definition???

Monad has to be Applicative???

My, my, my! The world does change quickly, doesn't it!
--}

userTweets :: [GraphJ] -> MMt
userTweets verse =
   let relates = concatMap rels verse
       users   = uniqueUsers verse
       tweets  = Map.fromList (map (index &&& id) (Set.toList (uniqueTweets verse)))
   in  foldr (addRow users tweets) (MM.MM Map.empty Set.singleton) relates

-- So, for each user, filter the relations of that user that also contains
-- a tweet id ... so tweets are better in a map

-- or, put another way, for each relation, if the relation has a tweet id and
-- a user id add that to the multimap in the maybe monad.

-- so, how to add a maybe key and a maybe value to a multimap ... justly?

type UserMap = Map String User
type TweetMap = Map String (Tweet String)

addRow :: UserMap -> TweetMap -> RelJ -> MMt -> MMt
addRow users tweets rel mm =
   fromMaybe mm (usertweet users tweets rel >>= \(u,t) -> 
                 return (MM.insert u t mm))  -- liftM/flip/uncurry... something

-- well, we need to know if there is a user-tweet relation!

usertweet :: UserMap -> TweetMap -> RelJ -> Maybe (User, Tweet String)
usertweet users tweets (RJ _ _ start end _) =
    liftM2 (,) (finduser start) (findtweet end)
    `mplus` liftM2 (,) (finduser end) (findtweet start)

-- which involves looking up values in the individual maps

      where findtweet = flip Map.lookup tweets
            finduser  = flip Map.lookup users

{--
*Y2016.M08.D19.Solution> let mm = userTweets rows
*Y2016.M08.D19.Solution> length (MM.store mm) ~> 7
*Y2016.M08.D19.Solution> let ans = Map.mapKeys name (Map.map length (MM.store mm))
*Y2016.M08.D19.Solution> mapM_ print (Map.toList ans)
("1HaskellADay",11)
("Aaron Levin",1)
("Amar Potghan",5)
("Edward Kmett",1)
("Francisco  T",1)
("Gabriel Gonzalez",1)
("geophf \217\8224",1)

TA-DAH!
--}

module Y2016.M11.D11.BonusSolution where

import System.Environment (getEnv)

-- available from 1HaskellADay git repository

import Control.Logic.Frege (natx)
import Data.Relation
import Graph.Query
import Graph.JSON.Cypher

import Y2016.M11.D08.Solution

{--
So, in honor of me solving today's problem yesterday, let's have a little
bonus exercise today, again, because of honors and bonuses and such-like.

Given that the followers of geophf, twitter id 81457442, are captured in JSON
at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D10/geophf.json

And the followers of 1HaskellADay, gzipped at the URL:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D08/1haskfollowersids.json.gz

which has an id_str of 2359501010 (I used users/lookup?screen_name=1HaskellADay
to confirm this).

Today's bonus problem. Read in the followers of geophf (as before) and the 
followers of 1HaskellADay (as before). Now, with these data, and using whatever
graphing tool you desire, plot the network of respective followers for each
of the examined tweeps (geophf and 1HaskellADay) and also plot or indicate
the shared followers of the two as a network.
--}

type IdTweep = (TwitterId, Tweeps)
type Name = String

data Twerp = T Name IdTweep
data TweepId = IdTw TwitterId

instance Node TweepId where
   asNode (IdTw x) = close $ asTweep x
instance Node Twerp where
   asNode (T n (i,_)) = close ("NAMED:" ++ asTweep i ++ ", name: " ++ show n)

asTweep :: TwitterId -> String
asTweep x = "TWEEP { twitterId: '" ++ show x ++ "'"

close :: String -> String
close x = x ++ " }"

data Follows = FOLLOWS deriving Show
instance Edge Follows where asEdge = show

twerpAsRel :: Twerp -> [Relation TweepId Follows Twerp]
twerpAsRel t@(T _ (_,followers)) =
   [Rel (IdTw x) FOLLOWS t | x <- tweeps followers]

networkFollowers :: Twerp -> Twerp -> IO String
networkFollowers tweep1 tweep2 =
   getEnv "CYPHERDB_ACCESS" >>=
   flip getGraphResponse (map (mkCypher "a" "rel" "b")
                              (natx (++) twerpAsRel tweep1 tweep2))

{--
So:
*Y2016.M11.D11.BonusSolution> followers "Y2016/M11/D08/1haskfollowersids.json" ~> had
*Y2016.M11.D11.BonusSolution> followers "Y2016/M11/D10/geophf.json" ~> geo
*Y2016.M11.D11.BonusSolution> networkFollowers (T "1HaskellADay" (2359501010, had)) (T "geophf" (81457442, geo)) ~> tons of json ending with ,\"errors\":[]}\n"

YES!

Let's see the results! (captured as PNG files in this directory)
--}

-- now imagine this network extended by the followers of those followers, but
-- that is for another day. If you are in the USA, happy Veterans Day! And
-- have a great weekend, all!

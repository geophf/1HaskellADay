module Y2016.M11.D02.Exercise where

import Data.Map (Map)
import Data.Set (Set)

-- below import available from cabal (aeson)

import Data.Aeson

-- below imports available from 1HaskellADay git repository

import Graph.JSON.Cypher.Read.Graphs
import Data.Relation

import Y2016.M11.D01.Exercise  -- for User declaration

{--
Today we pull out the stops. Using the graph data from yesterday gzipped at:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D01/twitter-users.json.gz

parse the JSON to get the FOLLOWS-relations between the users participating
with @1HaskellADay
--}

type Followings = Map User (Set User)

itfollows :: FilePath -> IO Followings
itfollows json = undefined

-- hint: above Graph import may help in this definition

-- so that if you query on "1HaskellADay" user-name, you get all the users that
-- account follows.

-- Now. You may not see all your followers if you query yourself. That's fine.
-- We'll deal with that in the BONUS

-- 1. Who do you follow who follows you back?

ilikeu :: Followings -> Name -> Set User
ilikeu = undefined

-- 2. Who follows you who you follow back? Same list?

ulikeme :: Followings -> Name -> Set User
ulikeme = undefined

-- 3. Who do you follow who does not follow you back?

frenemies :: Followings -> Name -> Set User
frenemies = undefined

-- 4. Who follows you that you do not follow back?

whoareu :: Followings -> Name -> Set User
whoareu = undefined

{-- BONUS -----------------------------------------------------------------

You may not see all your followers or all theirs, etc, that is because this
is the graph of users who interact on @1HaskellADay. If you want to see your
own graph, go to URL:

http://network.graphdemos.com

Sign in with your twitter id, and it will create a graph of your tweets and your
twitter social network.

Enter the query

match path=(u1:User)-[:FOLLOWS]->(u2:User) return path

download that as JSON, and then you can answer the above questions on your
own twitter network.

Do that.
--}

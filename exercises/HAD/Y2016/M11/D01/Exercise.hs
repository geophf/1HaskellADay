module Y2016.M11.D01.Exercise where

import Data.Map (Map)

-- Data.Aeson can be obtained from cabal ... which are foes in Destiny, but okay

import Data.Aeson

{--
The theme for this week is 1) Friendzoned, and 2) DYI

We have twitter-data-as-graph zipped at this directory or is downloadable via
the URL:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D01/twitter-users.json.gz

It contains the JSON-structure of users who follow users on 1HaskellADay.

Ingest these data, for now, just the users, and answer the below.
--}

type Name = String
data User = SomeStructureYouDeclare
   deriving Show

instance FromJSON User where
   parseJSON = undefined

type Users = Map Name User

ingestUsers :: FilePath -> IO Users
ingestUsers = undefined

-- 1. List the users by people who follow them, most followed user first

afollowing :: Users -> [User]
afollowing = undefined

-- 2. List the users by people they follow, with the user who follows the most first

afollower :: Users -> [User]
afollower = undefined

-- 3. List the users by the ratio of followers to following. You figure out what
-- ratio and which ordering makes sense to you for your data mining wizardry.

arationing :: Users -> [User]
arationing = undefined

{--
Now, why the DYI-theme this week?

I intended to publish this problem yesterday, but neo4j's twitter docker-app
was broken, and it took them the day to fix it. Hm. BUT, they have their source
code on git, so why not just connect to the Twitter-API via Haskell, download
the relevant data and create Haskell forms from that, instead of via the neo4j-
app?

Why not indeed?

So, this week, we will also be looking at doing that.
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D20.Exercise where

{--
So, yesterday we output our Regroup as a CSV and then showed it as a stacked
chart. The thing is, I wasn't particularly happy with the representation.

Well, in modeling the Regroup off the Archive-type, it got me thinking of 
graph theory again, and so: why not graphs?

Why not, indeed!

So, today's Haskell problem is to regroup our data then pick a week (just to
get a slice) to represent the view in the graph.

HOW DO WE DO THIS?

Well, like Shirlock Holmes, it's elementary! Follow the imports, my dear
Dr. Watson!
--}

import Data.Map (Map)
import Data.Time

-- below imports via 1HaskellADay git repository

import Data.Archive
import Data.Relation
import Graph.Query

-- so, from:

import Y2017.M10.D04.Exercise

-- we get our grouping. Turn that grouping into archived articles by topic
-- (by day, of course)

type Articles = Archive (Map Topic ArticleSummary)

articlesByTopic :: Grouping -> Articles
articlesByTopic grp = undefined

-- Cool beans! Now that you have that archive, you see that we can build
-- relations between days of the archive and put that into a graph database.

-- But we don't have that facility for the topicality of articles. So:

artsRels :: Map Topic ArticleSummary -> [Relation a b c]
artsRels topics = undefined

-- that means you have to declare types a, b, and c. See Data.Relation for help

-- Also, we have to define the relations from days to the topics containing
-- the articles:

topicRels :: Day -> Map Topic a -> [Relation Date b c]
topicRels day topics = undefined

-- see what a Date is in Data.Archive

-- With all that, define the relations of the entire archive:

archRels :: Articles -> [Relation a b c]
archRels arts = undefined

-- Again, you have to declare types a, b, and c

-- Hint: You MAY want types a,b,c to be the same throughout. How do you make
-- that work?

{-- BONUS -----------------------------------------------------------------

Upload a slice of these relations to a graph database. View your graph.
How many relations did you upload? What was the topic with the most articles
in one of the days in your graph?

--}

uploadArticles2Graph :: Endpoint -> Articles -> IO ()
uploadArticles2Graph url arts = undefined

{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2017.M10.D20.Solution where

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
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time
import Database.PostgreSQL.Simple (connect, close)
import System.Environment (getEnv)

-- below imports via 1HaskellADay git repository

import Data.Archive
import Data.Relation
import Graph.JSON.Cypher
import Graph.Query
import Store.SQL.Connection (connectInfo)

-- so, from:

import Y2017.M10.D04.Solution
import Y2017.M10.D17.Solution (marshalGrouping)

-- we get our grouping. Turn that grouping into archived articles by topic
-- (by day, of course)

type Articles = Archive (Map Topic [ArticleSummary])

-- just a very slight difference from yesterday: we do not reduce
-- [ArticleSummary] to its length

articlesByTopic :: Grouping -> Articles
articlesByTopic = foldr populateMap Map.empty . Map.toList

-- structure is: [(topic1, mapping day -> [arts]), ...]

-- regroups the group from a topic-centric bias to a day-centric one
-- and turns the list of articles (in this case per topic per day) to a count

populateMap :: (Topic, Map Day [ArticleSummary]) -> Articles -> Articles
populateMap (top, Map.toList -> days) = flip (foldr (appendDay top)) days

appendDay :: Topic -> (Day, [ArticleSummary]) -> Articles -> Articles
appendDay top (day, arts) regrp =

-- Let's think about this for a second.

-- The topic for the day is unique.

-- so: there's no 'append' per se; it's an insert ... correct?

   Map.insert day (Map.insert top arts topmap) regrp
      where topmap = fromMaybe Map.empty (Map.lookup day regrp)

-- Cool beans! Now that you have that archive, you see that we can build
-- relations between days of the archive and put that into a graph database.

-- But we don't have that facility for the topicality of articles. So:

artsRels :: Map Topic [ArticleSummary] -> [ArchiveRelation]
artsRels = concatMap eachTopic . Map.toList

-- that means you have to declare types a, b, and c. See Data.Relation for help

type ArchiveRelation = Relation ArchiveNode ArchiveRel ArchiveNode

data ArchiveNode = DayNode Date | TopicNode Topic Int | ArticleNode ArticleSummary
   deriving (Eq, Show)

instance Node ArchiveNode where
   asNode (DayNode (Date d)) = "Date { date: '" ++ show d ++ "' }"
   asNode (TopicNode tp sz)  = "Topic { topic: '" ++ tp ++ "', size: " ++ show sz ++ " }"
   asNode (ArticleNode (ArtSum idx title _)) = "Article { idx: " ++ show idx
         ++ ", title: '" ++ clean title ++ "' }"

clean :: String -> String
clean = filter (/= '\'')

data ArchiveRel = TOPICALITY | ABOUT deriving (Eq, Show)

instance Edge ArchiveRel where asEdge = show

eachTopic :: (Topic, [ArticleSummary]) -> [ArchiveRelation]
eachTopic (top, arts) =
   map (Rel (TopicNode top (length arts)) ABOUT . ArticleNode) arts

-- Also, we have to define the relations from days to the topics containing
-- the articles:

topicRels :: Day -> Map Topic [ArticleSummary] -> [ArchiveRelation]
topicRels day topics =
   map (Rel (DayNode (Date day)) TOPICALITY . uncurry TopicNode)
       (Map.toList (Map.map length topics))
   ++ artsRels topics

-- see what a Date is in Data.Archive

-- With all that, define the relations of the entire archive:

archRels :: Articles -> [ArchiveRelation]
archRels = concatMap (uncurry topicRels) . Map.toList

-- Again, you have to declare types a, b, and c

-- Hint: You MAY want types a,b,c to be the same throughout. How do you make
-- that work?

{-- BONUS -----------------------------------------------------------------

Upload a slice of these relations to a graph database. View your graph.
How many relations did you upload? What was the topic with the most articles
in one of the days in your graph?

--}

uploadArticles2Graph :: Endpoint -> Articles -> IO String
uploadArticles2Graph url =
   getGraphResponse url . map (mkCypher "a" "rel" "b") . archRels

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> (tops, grp) <- marshalGrouping conn (read "2017-08-28") (read "2017-09-03") 10
>>> close conn
>>> mapM_ print tops
("Social networks",98)
("Floods",79)
("Presidents",68)
("Hurricanes",46)
("Rain",42)
("Books",39)
("Tennis",32)
("Tournaments & championships",26)
("Art galleries & museums",24)
("Motion pictures",23)

>>> uploadArticles2Graph graphURL (articlesByTopic grp)
--}

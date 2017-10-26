{-# LANGUAGE MultiParamTypeClasses #-}

module Y2017.M10.D25.Solution where

{--
Today, we're going to divide the topic you chose into subtopics, and then
optionally, view those subtopics as a graph, or retrieve articles in the 
subtopic by index (which also works in the graph-structure, too, but in this
case the graph is specialized to a map.

Okay, yesterday, we computed the subtopics by counting instances, but today,
we want the more general grouping of subtopics by object.
--}

import Control.Arrow ((&&&))
import Control.Monad
import Data.List (isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)

-- below imports available via 1HaskellADay git repository.

-- import Control.Logic.Frege ((-|))
import Data.Hierarchy
import Data.Relation

import Graph.Query (cyphIt, graphEndpoint, Endpoint)

import Y2017.M10.D04.Solution (ArticleSummary (ArtSum), Topic)
import Y2017.M10.D05.Solution (visualize)
import Y2017.M10.D20.Solution  -- for article groupings and graphing
import Y2017.M10.D23.Solution  -- Article
import Y2017.M10.D24.Solution  -- to get the articles we're studying from file
   hiding (hasSubcategory, categorize, seed, categorizor)

-- So, we want to subcategorize articles using the categorizer, but grab
-- the whole article, not just a count

-- First lets map the articles for indexing by their, well, index:

type IxArts = Map Integer ArticleSummary

indexArticles :: [Article] -> IxArts
indexArticles = Map.fromList . map (artIdx &&& summarize)

-- you fetch the articles from file using articlesFromFile

-- but that also means we need to translate Article values to ArticleSummary

summarize :: Article -> ArticleSummary
summarize art = ArtSum (artIdx art) (title art) (published art)

-- now that we have our articles nicely indexed, let's (sub)categorize them

hasSubcategory :: MonadPlus m => String -> Article -> m (IxArts -> IxArts)
hasSubcategory storm art =
   guard (isInfixOf storm (fullText art)) >> inserter art

inserter :: MonadPlus m => Article -> m (IxArts -> IxArts)
inserter art = pure (Map.insert (artIdx art) (summarize art))

-- adds this article to, e.g.: "Maria" subcategory if it mentions, e.g.: "Maria"

-- which means we need a starter function:

seed :: Subcategory IxArts
seed = replicate 4 Map.empty

-- now we can categorize hurricane articles by each storm
-- or, more generally, categorize articles into subcategories

categorizor :: [Article] -> Subcategory IxArts
categorizor = foldr (\art ->
   let maria  = hasSubcategory "Maria" art
       harvey = hasSubcategory "Harvey" art
       irma   = hasSubcategory "Irma" art
       none   = case msum [maria, harvey, irma] of

-- case msum is saying: "Are all of these mzero?"
-- And now we need monadic xor here:

                     Nothing -> inserter art
                     Just _  -> Nothing

   in  zipWith (\m a -> fromMaybe a (m <*> pure a))
               [maria, harvey, irma, none]) seed

-- puts each article into its respective subcategory bin
-- n.b. an article can be in multiple bins.

-- With the data sets you derived from the NYT archive, take the category
-- you chose and break it into subcategories that make sense.

-- Now you've got groups of related content you can browse.

{-- BONUS -----------------------------------------------------------------

Create a data visualization from your results, for example as a circle chart
or as a graph

E.g. use Y2017.M10.D05.Exercise.visualize or Y2017.M10.D20.uploadArticles2Graph
--}

-- Okay. Here we go.

data ArticleNode = Root | Subject Topic | SubTopic Topic Int | News ArticleSummary
   deriving (Eq, Show)

instance Node ArticleNode where
   asNode Root = "ARCHIVE { name: 'NYT' }"
   asNode (Subject top) = "TOPIC { name: '" ++ top ++ "' }"
   asNode (SubTopic top n) =
       "SUBTOPIC { name: '" ++ top ++ "', children: " ++ show n ++ " }"
   asNode (News (ArtSum a b c)) =
       "ARTICLE { id: " ++ show a ++ ", title: '" ++ clean b 
           ++ "', published: '" ++ show c ++ "' }"

data ArticleRel = TOPIC | SUBTOPIC | ARTICLE deriving (Eq, Show)

instance Edge ArticleRel where asEdge = show

instance Relatable ArticleNode ArticleNode ArticleRel where
   relate Root a@(Subject _)               = Rel Root TOPIC a
   relate a@(Subject _) b@(SubTopic _ _)   = Rel a SUBTOPIC b
   relate a@(SubTopic _ _) b@(News _)      = Rel a ARTICLE b

-- So, now, we convert our map of topics to subtopics (and their articles)
-- to a hierarchy and graph.

arts2hier :: Map Topic (Map Topic [ArticleSummary]) -> Hierarchy ArticleNode
arts2hier = Hier Root . Kids . map mkTopicHier . Map.toList

mkTopicHier :: (Topic, Map Topic [ArticleSummary]) -> Hierarchy ArticleNode
mkTopicHier (top, arts) =
   Hier (Subject top) (Kids (map mkSubtopics (Map.toList arts)))

mkSubtopics :: (Topic, [ArticleSummary]) -> Hierarchy ArticleNode
mkSubtopics (top, arts) =
   Hier (SubTopic top (length arts)) (Kids (map (flip Hier (Size 1) . News) arts))

graphCategories :: Endpoint -> Map Topic (Map Topic [ArticleSummary]) -> IO String
graphCategories url = cyphIt url . hier2rel . arts2hier

-- which means we have to load in the topics then map topics to subtopics and
-- subtopics to articles

{--
>>> hurr <- articlesFromFile "Y2017/M10/D24/hurricanes.json.gz" 
>>> title $ head hurr
"In Sweltering South, Climate Change Is Now a Workplace Hazard"

>>> flood <- articlesFromFile "Y2017/M10/D24/floods.json.gz" 
>>> rain <- articlesFromFile "Y2017/M10/D24/rains.json.gz"  

>>> rainCat = categorizor rain
>>> floodCat = categorizor flood
>>> hurrCat = categorizor hurr

>>> subCat2Map = Map.fromList . zip ["Maria", "Harvey", "Irma", "None"] . map Map.elems 
>>> megaMap = Map.fromList (zip ["Hurricanes", "Floods", "Rain"] (map subCat2Map [hurrCat, floodCat, rainCat]))

>>> url <- graphEndpoint 
>>> graphCategories url megaMap
... \"errors\": []}

And we have our graph. YES!

Let's formalize the above (for hurricanes, anyway):
--}

subCat2Map :: Subcategory IxArts -> Map Topic [ArticleSummary]
subCat2Map = Map.fromList . zip (words "Maria Harvey Irma None") . map Map.elems

hurricaneAnalysis :: IO String
hurricaneAnalysis = do
   hurr <- articlesFromFile "Y2017/M10/D24/hurricanes.json.gz"
   flood <- articlesFromFile "Y2017/M10/D24/floods.json.gz"
   rain <- articlesFromFile "Y2017/M10/D24/rains.json.gz"
   let rainCat = categorizor rain
       floodCat = categorizor flood
       hurrCat = categorizor hurr
       megaMap = Map.fromList (zip (words "Hurricanes Floods Rain")
                                   (map subCat2Map [hurrCat, floodCat, rainCat]))
   url <- graphEndpoint
   graphCategories url megaMap

{--
>>> hurricaneAnalysis 
...,{\"columns\":[],\"data\":[]}],\"errors\":[]}\n"

... in the neo4j browser:

match (t:TOPIC { name: 'Hurricanes' })-[]->(s:SUBTOPIC {name: 'Harvey'})-[]->(a:ARTICLE)
return a.id, a.published, a.title ORDER BY a.published DESC

a.id	a.published	a.title
7729	2017-09-23	When Disaster Hits and Landlines Fail, Social Media Is a Lifeline
7570	2017-09-22	How to Help Puerto Rico and Other Islands After Hurricane Maria
7454	2017-09-21	How to Avoid Buying a Car Flooded by Hurricanes
7024	2017-09-19	The 2017 Hurricane Season Really Is More Intense Than Normal
7048	2017-09-19	Harvey and Irma Wiped Out Our Kitchens. Still, We Cook.
6873	2017-09-18	How the Internet Kept Humming During 2 Hurricanes
6875	2017-09-18	The Real Unknown of Climate Change: Our Behavior
...

GRAPH, HO!

Here's how to read an article from the graph:

>>> hurmap = Map.fromList (map (artIdx &&& fullText) hurr)
>>> hurmap Map.! 3225

and there's article 3225
--}

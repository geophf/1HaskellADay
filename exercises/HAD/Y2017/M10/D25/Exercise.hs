module Y2017.M10.D25.Exercise where

{--
Today, we're going to divide the topic you chose into subtopics, and then
optionally, view those subtopics as a graph, or retrieve articles in the 
subtopic by index (which also works in the graph-structure, too, but in this
case the graph is specialized to a map.

Okay, yesterday, we computed the subtopics by counting instances, but today,
we want the more general grouping of subtopics by object.
--}

import Data.Map (Map)
import Data.Set (Set)

-- below imports available via 1HaskellADay git repository.

import Graph.Query (cyphIt)

import Y2017.M10.D04.Exercise (ArticleSummary(ArtSum), Topic)
import Y2017.M10.D05.Exercise (visualize)
import Y2017.M10.D20.Exercise  -- for article groupings and graphing
import Y2017.M10.D23.Exercise  -- Article
import Y2017.M10.D24.Exercise  -- to get the articles we're studying from file
   hiding (hasSubcategory, categorize, seed)

-- So, we want to subcategorize articles using the categorizer, but grab
-- the whole article, not just a count

-- First lets map the articles for indexing by their, well, index:

type IxArts = Map Integer ArticleSummary

indexArticles :: [Article] -> IxArts
indexArticles arts = undefined

-- you fetch the articles from file using articlesFromFile

-- but that also means we need to translate Article values to ArticleSummary

summarize :: Article -> ArticleSummary
summarize art = undefined

-- now that we have our articles nicely indexed, let's (sub)categorize them

hasSubcategory :: String -> Article -> (Bool, IxArts -> IxArts)
hasSubcategory storm art = undefined

-- adds this article to, e.g.: "Maria" subcategory if it mentions, e.g.: "Maria"

-- which means we need a starter function:

seed :: Subcategory IxArts
seed = undefined

-- now we can categorize hurricane articles by each storm
-- or, more generally, categorize articles into subcategories

categorizor :: [Article] -> Subcategory IxArts
categorizor arts = undefined

-- puts each article into its respective subcategory bin

-- With the data sets you derived from the NYT archive, take the category
-- you chose and break it into subcategories that make sense.

-- Now you've got groups of related content you can browse.

{-- BONUS -----------------------------------------------------------------

Create a data visualization from your results, for example as a circle chart
or as a graph

E.g. use Y2017.M10.D04.Exercise.visualize or Y2017.M10.D20.uploadArticles2Graph
--}

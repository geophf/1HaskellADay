module Y2018.M06.D18.Exercise where

{--
Yesterday we wrote a SQL DELETE statement, but, NOT SO FAST! Yesterday
we discovered duplicates in the data set, ... but are there 'non-dpulicates'?

Let's find out today.
--}

import Data.Map (Map)

-- below imports available via 1HaskellADay git repository

import Y2018.M06.D15.Exercise

{-- First load in the article set and sort them.
>>> arts <- readArticleDuplicates (exDir ++ tsvFile)
>>> sorts <- sortedArticles arts
--}

-- Question 1: how many UUIDs have more than 2 articles?

type MapAArts = Map ArticleId [Article]

moreThan2 :: MapAArts -> MapAArts
moreThan2 sorts = undefined

{--
If you look at the above results you see that the UUIDs that have more than
two articles the articles for each of the UUIDs have different published
dates. That leads me to believe there are UUIDs that have more than one
article but these articles for the same UUID are different.

Talk about misuse of a datatype! "I have an UUID!" "I have one, too, but I was
published nearly a year later!"

Fancy that, Hedda!

Well, this analysis worked for the moreThan2 set, ... how about in general?
--}

nonDuplicate :: MapAArts -> MapAArts
nonDuplicate sorts = undefined

-- Save out these non-duplicates as uuid,id,publish_dt-CSV file

nonDuplicateReport :: FilePath -> MapAArts -> IO ()
nonDuplicateReport toFile sorted = undefined

-- and present to management and say: "So, what're we gonna do with these, boss?"

-- ... they love* it when you present findings like these. Trust me; I know.

-- And whilst management is wringing their hands and will eventually say:

-- "Well, you do something. Figure it out! You're the Expert(tm)"

-- You regenerate the SQL DELETE statement, but this time without the
-- non-duplicate ids

-- Question: how many ids in total are there in arts? How many duplicate IDs
-- are you eliminating?

ids :: [Article] -> [Idx]
ids arts = undefined

dupIds :: [Article] -> [Idx]
dupIds arts = undefined

-- there should be fewer duplicate IDs than IDs in extracted set.

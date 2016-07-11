module Y2016.M07.D11.Exercise where

import Data.Aeson

import RID.Tree

{--
We have the RID / Regressive Imagery Dictionary 'magically' saved out as JSON
(as we have done this in Haskell exercises before ... a long, long time ago) at
rid.json at this directory or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M07/D11/rid.json

Wouldn't it be nice to be able to read in the RID as JSON?

Today's Haskell exercise read and materialize the RID from JSON
--}

ridFromJSON :: FilePath -> IO RID
ridFromJSON = undefined

-- Hint: Data.Aeson can decode JSON to Haskell structures.

{--
We know that the cognitions of the words are: PRIMARY, SECONDARY, and EMOTIONS.
List out the categories, given the RID and a cognition.
--}

catsFor :: RID -> Cognition -> [Kinds] -- particularly, just the Catg's
catsFor = undefined

-- So all three cognitions have categories (how many for each cognition?)
-- How many SUB-categories does the primary cognition have? List them out
-- by (category, number of subcategories for that category)

subcats :: RID -> [(Kinds, Int)] -- again, primary categories is the Kinds value
subcats = undefined

{--
This week we are going to be focusing on looking at the meaning of documents.
The RID is one approach, the WordNet is another, and there are others. We'll
dive into the RID first.
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D03.Solution where

import Control.Arrow ((&&&))
import Data.List (stripPrefix)
import Data.Map (Map, fromList)
import Data.Maybe (mapMaybe)

-- the below import is available via 1HaskellADay git repository

import Y2018.M04.D02.Solution

{--
Okay, yesterday we discovered some structure to the sample JSON (see the import
of yesterday's import). Today, we're going to go into the text of the articles-
as-JSON, that is to say: the unstructured data, and extract the author ... if
there is an author to extract.

From yesterday's arts, load in the articles into the following structure
(which I declared in yesterday's solution).

>>> (Success ans) = (fromJSON json) :: Result [Article']
>>> idx (head ans)
22378
>>> date (head ans)
Just 2018-03-30 15:05:35 +0000

But note: we don't have author. As we say from yesterday's results, the author
key-value pair is meaningless. But the content MAY have an author by-line.

Discover where the author by-line is in the content of the JSON from yesterday,
then, extract the author information from it, if there is an author for the
article.
--}

type Author = String

author :: Article -> Maybe Author
author = authorish . head . lines . plain 

authorish :: String -> Maybe Author
authorish = fmap trim . stripPrefix "By "

trim :: String -> String
trim line | last line == ' ' = trim (init line)
          | otherwise        = line

{--
>>> (Success arties) = (fromJSON json) :: Result [Article]
>>> map author arties
[Just "Ahmed H. Adam",Just "Jonathan Cristol",Nothing,Nothing,Nothing]
--}

-- from the JSON from yesterday, provide a mapping from article idx to 
-- article author. A Nothing for author means that the article doesn't have
-- one (so will not be inserted into the map).

type Idx = Int

authors :: [Article] -> Map Idx Author
authors = fromList . mapMaybe (sequence . (idx . art &&& author))

{--
>>> authors arties
fromList [(22301,"Jonathan Cristol"),(22378,"Ahmed H. Adam")]

Now, tomorrow we'll look at archiving these articles in a PostgreSQL database.
--}

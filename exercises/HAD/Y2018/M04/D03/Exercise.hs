{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D03.Exercise where

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Map (Map)
import Data.Time

-- imports are available via 1HaskellADay git repository

import Data.HTML

import Y2018.M04.D02.Exercise (arts)

{--
Okay, yesterday we discovered some structure to the sample JSON (see the import
of yesterday's import). Today, we're going to go into the text of the articles-
as-JSON, that is to say: the unstructured data, and extract the author ... if
there is an author to extract.

From yesterday's arts, load in the articles into the following structure
--}

data Article' =
   Art' { idx                     :: Int,
          date                    :: Maybe ZonedTime,
          title, excerpt, content :: Value,
          tags, categories        :: [Int],
          link                    :: FilePath }
      deriving Show

instance FromJSON Article' where
   parseJSON obj = undefined

{--
>>> (Success ans) = (fromJSON json) :: Result [Article']
>>> idx (head ans)
22378

But note: we don't have author. As we say from yesterday's results, the author
key-value pair is meaningless. But the content MAY have an author by-line.

Discover where the author by-line is in the content of the JSON from yesterday,
then, extract the author information from it, if there is an author for the
article.
--}

type Author = String

author :: Article' -> Maybe Author
author art = undefined

-- from the JSON from yesterday, provide a mapping from article idx to 
-- article author. A Nothing for author means that the article doesn't have
-- one (so will not be inserted into the map).

type Idx = Int

authors :: [Article'] -> Map Idx Author
authors arts = undefined

-- How many articles have authors? Which article idx values have authors?

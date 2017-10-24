{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D24.Exercise where

{--
So, yesterday, we loaded in a slice of the NYT archive then saved it off as
JSON (the articles, proper) and haskell lists (the subject-linking information).

So, what are we going to do today?

Load back in that slice, obviously.
--}

import qualified Codec.Compression.GZip as GZ
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

-- below import available via 1HaskellADay git repository

import Y2017.M10.D23.Exercise

-- first up: load in the subject linking information:

artIdsBySubj :: FilePath -> IO [Integer]
artIdsBySubj file = undefined

-- Now load in the articles, themselves, from the compressed archive
-- (my compressed archive is around the topic of hurricanes and floods)

instance FromJSON Article where
   parseJSON art = undefined

articlesFromFile :: FilePath -> IO [Article]
articlesFromFile file = undefined

{--
Now that you have the articles by the topic you chose, partition the article
by subtopic, ... a bit of triage. For me, I choose "Hurricanes" so I'm 
partitioning articles by "Maria" "Harvey" "Irma" and none of those. For the
topic you choose you 'may' (and for this exercise 'may' means 'shall') choose
to partition your topic into subtopics (one of which being a catch-all.

How do we do this?

I'm thinking applicative functors, ... or something like them ...
--}

type Subcategory a = [a]

seed :: Subcategory Int
seed = [0,0,0,0] -- Maria, Harvey, Irma, none

-- For each article that mentions Maria, increment the Maria-count.
-- For each article that mentions Harvey, ...

-- etc, etc, and you get me

hasSubcategory :: String -> Article -> (Int -> Int)
hasSubcategory hurr art = undefined

-- from that, we get an applicative functor of hurricanes that we can <*>

-- How many articles do you have? How many of each subtopic do you have?
-- How many catchalls do you have (that is, no categorization?)

categorizor :: [Article] -> Subcategory Int
categorizor arts = undefined

-- And, finally, write out a report in human readable form of your subcategories

reportSubcategories :: Show a => Subcategory a -> IO ()
reportSubcategories cats = undefined

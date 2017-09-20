{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D14.Solution where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

{--
This week we have been working on encoding and decoding strings to ints and
back, using a 'dictionary,' or mapping between to the two to facilitate these
translations.

Today we're going to take it up a notch.
--}

import Y2017.M09.D14.Exercise

{--
The above import was an exercise to store article text, and some metadata about
the articles in a PostgreSQL data store. Today, using these articles, we're
going to rip and to rend them apart into keywords...
--}

import Y2017.M09.D15.Exercise

{--
And then store these keywords into a dictionary AND a keyword-strength store...
ON LINE!

Ooh! How exciting!

Okay, let's get to it.

The keyword table in the database is structured thus:
--}

data Key = Key { keyId :: Int, keyword :: String }
   deriving (Eq, Ord, Show)

-- Given an insert statement for a new keyword is:

insertKeyStmt :: Query
insertKeyStmt = [sql|INSERT INTO keyword (id, keyword) VALUES (?,?)|]

-- Question 1: what is its ToRow instance definition?

instance ToRow Key where
   toRow kw = undefined

{--
Today we're going to be tying together the results of Y2017.M09.D15.Exercise's
keyword extraction with the data store, so we also need a keywords-with-their-
strengths-per-article table, and this we call article_keyword in the database.

It is structured thus:
--}

data ArticleKeyWord = AKW { artId, kwId :: Int, kw :: KeyWord }
      deriving (Eq, Ord, Show)

-- given the insert statement for a keyword for an article is:

insertAKWStmt :: Query
insertAKWStmt = [sql|INSERT INTO article_keyword
                     (article_id, keyword_id, keyword_count, keyword_strength)
                     VALUES (?,?,?,?)|]

-- Question 2: what is ArticleKeyWord's ToRow instance definition?

instance ToRow ArticleKeyWord where
   toRow akw = undefined

-- Question 3: the biggy

{--
Let's say the results of the query

select id from article where article_id = 'AP900327-0094.txt';

against the data store returns the id = 45. 

1. populate the keyword table with the dictionary resulting from

>>> wordcontext <- kea 0 Map.empty <$> BL.readFile testFile
--}

insertAllKeys :: Connection -> Dictionary -> IO ()
insertAllKeys conn dict = undefined

-- the above function assumes an empty keyword table

-- hint: you may wish to convert the Dictionary value to [Key]

-- then 2. insert all the keywords for AP900327-0094.txt into article_keyword

insertAllArticleKeyWords :: Connection -> Map Int KeyWord -> IO ()
insertAllArticleKeyWords conn kws = undefined

-- hint: you may wish to convert kws to [ArticleKeyWord]

-- We'll look at reading from these tables tomorrow.

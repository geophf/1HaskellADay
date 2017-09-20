{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D20.Solution where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Store.SQL.Connection (connectInfo)

{--
This week we have been working on encoding and decoding strings to ints and
back, using a 'dictionary,' or mapping between to the two to facilitate these
translations.

Today we're going to take it up a notch.
--}

import Y2017.M09.D14.Solution

{--
The above import was an exercise to store article text, and some metadata about
the articles in a PostgreSQL data store. Today, using these articles, we're
going to rip and to rend them apart into keywords...
--}

import Y2017.M09.D15.Solution

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
   toRow (Key k v) = [toField k, toField v]

{--
Today we're going to be tying together the results of Y2017.M09.D15.Exercise's
keyword extraction with the data store, so we also need a keywords-with-their-
strengths-per-article table, and this we call article_keyword in the database.

It is structured thus:
--}

data ArticleKeyWord = AKW { artId :: Int, kw :: KeyWord }
      deriving (Eq, Ord, Show)

-- given the insert statement for a keyword for an article is:

insertAKWStmt :: Query
insertAKWStmt = [sql|INSERT INTO article_keyword
                     (article_id, keyword_id, keyword_count, keyword_strength)
                     VALUES (?,?,?,?)|]

-- Question 2: what is ArticleKeyWord's ToRow instance definition?

instance ToRow ArticleKeyWord where
   toRow (AKW art (KW key cnt str)) =
      let strength = (fromRational str) :: Float in
      [toField art, toField key, toField cnt, toField strength]

-- Question 3: the biggy

{--
Let's say the results of the query

select id from article where article_id = 'AP900327-0094.txt';

against the data store returns the id = 2

1. populate the keyword table with the dictionary resulting from

>>> wordcontext <- kea 0 Map.empty <$> BL.readFile testFile
--}

insertAllKeys :: Connection -> Dictionary -> IO ()
insertAllKeys conn =
   inserter conn insertKeyStmt . map (uncurry Key . swap) . Map.toList

inserter :: ToRow a => Connection -> Query -> [a] -> IO ()
inserter conn = void <<- executeMany conn

-- the above function assumes an empty keyword table

-- hint: you may wish to convert the Dictionary value to [Key]

{--
>>> connectInfo 
ConnectInfo {connectHost = "pellefant.db.elephantsql.com", connectPort = 5432, connectUser = "upksmrvp", connectPassword = "k_3U6Tcgv5gRY_PoX_AHOekD4khtyYf8", connectDatabase = "upksmrvp"}
>>> conn <- connect it
>>> length (dict wordcontext)
91
>>> insertAllKeys conn (dict wordcontext)

Now, in the database:

select count(1) from keyword;
91

So the insert worked! Let's take a look:

select * from keyword LIMIT 5;

id	keyword
---------------
37	a
25	about
10	after
16	agency
73	am

shows the first 5 keywords stored in the database. 

n.b., they are stored alphabetically.
--}

-- then 2. insert all the keywords for AP900327-0094.txt into article_keyword

insertAllArticleKeyWords :: Connection -> Int -> Map Int KeyWord -> IO ()
insertAllArticleKeyWords conn artId =
   inserter conn insertAKWStmt . map (AKW artId) . Map.elems

-- hint: you may wish to convert kws to [ArticleKeyWord]

{--
>>> length (kws wordcontext)
91
>>> insertAllArticleKeyWords conn 2 (kws wordcontext)

then at the database:

select * from article_keyword LIMIT 5;

id	article_id	keyword_id	keyword_strength	keyword_count
-----------------------------------------------------------------------------
1	2		0		0.07913669		11
2	2		1		0.02877698		4
3	2		2		0.007194245		1
4	2		3		0.021582734		3
5	2		4		0.007194245		1

WOOT!

>>> close conn
--}

-- We'll look at reading from these tables tomorrow.

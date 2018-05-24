{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D04.Solution where

{--
So you have the data from the last two day's exercises, let's start storing
those data into a PostgreSQL database. Today's exercise is to store just
the authors.

But there's a catch: you have to consider you're doing this as a daily upload.
So: are there authors already stored? If so we don't store them, if not, we
DO store them and get back the unique ID associated with those authors (for
eventual storage in an article_author join table.

First, fetch all the authors already stored (with, of course their unique ids)
--}

import Control.Monad.State

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- we will use previous techniques for memoizing tables:

-- below imports available via 1HaskellADay git repository

import Data.LookupTable
import Data.MemoizingTable (MemoizingS, MemoizingTable(MT))
import qualified Data.MemoizingTable as MT

import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import Y2018.M04.D02.Solution hiding (idx)
import Y2018.M04.D03.Solution
import Y2018.M04.D05.Solution -- looking forward in time, m'kay

-- 1. fetch the authors into a LookupTable then convert that into a memoizing
--    table state

{--
>>> json <- readJSON arts
>>> (Success arties) = (fromJSON json) :: Result [Article]
>>> ci <- connectInfo "WPJ"
>>> conn <- connect ci
--}

authorTableName :: String
authorTableName = "author"

lookupAuthors :: Connection -> IO LookupTable
lookupAuthors = flip lookupTable authorTableName

{--
>>> auths <- lookupAuthors conn
fromList []
--}

type MemoizedAuthors m = MemoizingS m Integer Author ()

lk2MS :: Monad m => LookupTable -> MemoizedAuthors m
lk2MS table = put (MT.start (map swap $ Map.toList table), Map.empty)

-- 2. from yesterday's exercise, triage the authors into the memoizing table

addNewAuthors :: Monad m => [Author] -> MemoizedAuthors m
addNewAuthors = MT.triageM 5 -- because '5' is a nice number

-- 3. store the new memoizing table values into the author table

authorStmt :: Query
authorStmt = [sql|INSERT INTO author (author) VALUES (?) returning id|]

data AuthorVal = AV Author

instance ToRow AuthorVal where
   toRow (AV x) = [toField x]

insertAuthors :: Connection -> MemoizedAuthors IO
insertAuthors conn = get                    >>= \(mt@(MT _ _ news), _) ->
   let authors = Set.toList news
       auths   = map AV authors in
   if null auths then return () else
   lift ((returning conn authorStmt auths) :: IO [Index]) >>= \idxen ->
   put (MT.update (zip (map idx idxen) authors) mt, Map.empty)

{--
>>> execStateT (addNewAuthors (Map.elems $ authors arties) >> 
         insertAuthors conn) (MT.start (map swap $ Map.toList auths), Map.empty)
(MT {fromTable = fromList [(1,"Ahmed H. Adam"),(2,"Jonathan Cristol")], 
     readIndex = fromList [("Ahmed H. Adam",1),("Jonathan Cristol",2)], 
     newValues = fromList []},fromList [])
>>> close conn

$ select * from author;

id	author
--------------
1	Ahmed H. Adam
2	Jonathan Cristol

--}

-- and there we go for today! Have at it!

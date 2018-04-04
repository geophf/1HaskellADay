{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D04.Exercise where

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

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- we will use previous techniques for memoizing tables:

-- below imports available via 1HaskellADay git repository

import Data.LookupTable
import Data.MemoizingTable

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import Y2018.M04.D02.Exercise (readJSON, arts)
import Y2018.M04.D03.Exercise (Author, authors)

-- 1. fetch the authors into a LookupTable then convert that into a memoizing
--    table state

authorTableName :: String
authorTableName = "author"

lookupAuthors :: Connection -> IO LookupTable
lookupAuthors conn = undefined

type MemoizedAuthors m = MemoizingS m Integer Author ()

lk2MS :: Monad m => LookupTable -> MemoizedAuthors m
lk2MS table = undefined

-- 2. from yesterday's exercise, triage the authors into the memoizing table

addNewAuthors :: [Author] -> MemoizedAuthors m
addNewAuthors authors = undefined

-- 3. store the new memoizing table values into the author table

authorStmt :: Query
authorStmt = [sql|INSERT INTO author (author) VALUES (?) returning id|]

insertAuthors :: Connection -> MemoizedAuthors IO
insertAuthors conn = undefined

-- and there we go for today! Have at it!

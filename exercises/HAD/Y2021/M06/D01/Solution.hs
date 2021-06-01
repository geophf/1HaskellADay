{-# LANGUAGE OverloadedStrings #-}

module Y2021.M06.D01.Solution where

import qualified Data.ByteString.Char8 as B
import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import Store.SQL.Util.TaggedTypes

import Store.SQL.Connection

{--
What is 'today'? When analyzing data, this can be a thorny question, like, for
example, you want to analyze some data, but, WHOOPS!, it just became 0000Z and
now it's 'tomorrow' computer-time, and now you don't have any data.

Ugh.

How to solve this?

EASY! If you have a table named 'transaction_log' and a column in that table 
named 'for_date,' then the query to get 'today' is:

today :: Connection -> IO Day
today conn = untag . head
         <$> tday conn "SELECT max(for_date) FROM transaction_log"
--}

tday :: Connection -> Query -> IO [TaggedType Day]
tday = query_

{--
>>> withConnection [database] (\conn -> today conn >>= print)
2021-06-01

That's fine if every database has the data we wish to analyze in
'transaction_log,' and the date column called 'for_date,' but hoping for both
is wistful thinking.

Generalize 'today' to 'latest' that takes a table name and column name and
gives you the latest date in that data-set.

n.b.: Building a Query-value is not trivial.
--}

type TableName = String
type ColumnName = String

latest :: Connection -> TableName -> ColumnName -> IO Day
latest conn tableName columnName =
   let qry = Query (B.pack ("SELECT max(" ++ columnName ++ ") FROM "
                           ++ tableName)) in
   untag . head <$> tday conn qry

-- rewrite 'today' in terms of 'latest.'

today :: Connection -> IO Day
today conn = latest conn "transaction_log" "for_date"

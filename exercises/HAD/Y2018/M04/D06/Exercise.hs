{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Y2018.M04.D06.Exercise where

import Data.Aeson

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Data.LookupTable

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

{--
Okay, we saved authors to the database, now we're going to save categories
to the database. But what are the categories?

I gotcher categories right here, fam:
--}

cats :: FilePath
cats = "Y2018/M04/D06/cats.json"

type Cat = String
type Category = IxValue Cat

instance FromJSON Category where
   parseJSON (Object o) = undefined

readCats :: FilePath -> IO [Category]
readCats file = undefined

-- now convert the list of categories to a LookupTable

cats2lk :: [Category] -> LookupTable
cats2lk cats = undefined

-- And, finally, store the lookup table into the database

catStmt :: Query
catStmt = [sql|INSERT INTO category VALUES (?, ?)|]

insertCats :: Connection -> LookupTable -> IO ()
insertCats conn table = undefined

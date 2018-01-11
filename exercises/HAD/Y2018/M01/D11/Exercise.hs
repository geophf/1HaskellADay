{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M01.D11.Exercise where

{--
Okay, this was supposed to be a different exercise, but I keep plumbing depths
to writing log entries to a data table, a supposedly simple and easy thing, but
here we are.

Today's Haskell exercise: now that we have lookup data tables in SQL, let's
translate them to lookup tables in Haskell.

Given a data table:

table a

id,value
1,bleh
2,bleu
3,blau
...

given the values are in Ord
given a fetch Query statement that returns Ord a => [IxValue a]

define a function that returns a Haskell lookup table
--}

import Data.Map (Map)
import Database.PostgreSQL.Simple

-- below import available via 1HaskellADay git repository

import Store.SQL.Util.Indexed

type LookupTable = Map String Integer

lookupTable :: Connection -> String -> IO LookupTable
lookupTable conn tableName = undefined

-- you may need this:

ix2lookup :: Show a => IxValue a -> (String, Integer)
ix2lookup ixval = undefined

-- there are two lookup tables we've dealt with in recent exercises:
-- severity_lk and action_lk: lookupTable those tables

-- the lookup table will be rolled into its own module

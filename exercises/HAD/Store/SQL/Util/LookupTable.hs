{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Store.SQL.Util.LookupTable where

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

import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

-- below import available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)

type LookupTable = Map String Integer

lookupTable :: Connection -> String -> IO LookupTable
lookupTable conn tablename =
   Map.fromList . map ix2lookup
     <$> query_ conn (Query (B.pack ("SELECT * FROM " ++ tablename)))

-- you may need this:

ix2lookup :: (Integer, String) -> (String, Integer)
ix2lookup = swap

-- there are two lookup tables we've dealt with in recent exercises:
-- severity_lk and action_lk: lookupTable those tables

{--
>>> withConnection (\conn -> lookupTable conn "severity_lk" >>= print)
fromList [("DEBUG",2),("ERROR",5),("FATAL",6),("INFO",3),("TRACE",1),("WARN",4)]

>>> withConnection (\conn -> lookupTable conn "action_lk" >>= print)
fromList [("DELETE",3),("INSERT",1),("UPDATE",2)]
--}

-- the lookup table will be rolled into its own module

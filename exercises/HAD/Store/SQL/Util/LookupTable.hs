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

import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Data.LookupTable (LookupTable)
import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as MT

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots (buildPivots)

lookupTable :: Connection -> String -> IO LookupTable
lookupTable conn = lookupTableFrom conn . Query . B.pack . ("SELECT * FROM " ++)

lookupTableFrom :: Connection -> Query -> IO LookupTable
lookupTableFrom conn query = Map.fromList . map ix2lookup <$> query_ conn query

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

-- The above retrieves lookup table information, now let's look at
-- (progressively) storing lookup information:

memoizeStoreM :: ToRow a => Ord a => Connection
              -> (Connection -> [a] -> IO [Index])
              -> Query -> (article -> [a]) -> [IxValue article]
              -> StateT (MemoizingTable Integer a) IO ()
memoizeStoreM conn storer pivotQuery getter ixarts =
   get >>= \mem ->
   let (ids,arts) = unzip (map ix2tup ixarts)
       infos = map getter arts
       stat = execState (zipWithM_ MT.triageM ids infos) (mem,Map.empty)
       substate = Set.toList (MT.newValues (fst stat))
   in  lift (storer conn substate) >>= \ixnewvals ->
   let table = MT.update (zip (map idx ixnewvals) substate) (fst stat)
       pivots = (evalState buildPivots (table, snd stat))
   in  lift (executeMany conn pivotQuery pivots) >> put table

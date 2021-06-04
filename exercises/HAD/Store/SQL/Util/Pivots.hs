{-# LANGUAGE OverloadedStrings #-}

module Store.SQL.Util.Pivots where

{-- Pivot tables ---------------------------------------------------------------

Last week you were able to scan the database, extract rows of name(s), parse
the names, then store them as parsed entities in the database connected to the
source article via a join-table. These join tables, also known as 'pivot
tables,' are prevalent and follow a certain structure. Instead of the specific
ArticlePersonJoin structure, we'll declare and use the general Pivot type here.
--}

import Control.Arrow (second)
import Control.Monad.State

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

-- below import available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingState, MemoizingTable(MT))
import qualified Data.MemoizingTable as MT

import Store.SQL.Util.Indexed

data Pivot = Pvt { srcIx, trgId :: Integer }
   deriving (Eq, Ord, Show)

-- creates a Pivot value from source and destination table indices

joinValue :: Indexed i => i -> Index -> Pivot
joinValue i j = Pvt (idx i) (idx j)

-- and a pivot as a pair:

toTup :: Pivot -> (Integer, Integer)
toTup (Pvt a b) = (a, b)

-- and now we need a pivot-inserter

instance ToRow Pivot where
   toRow (Pvt i j) = map toField [i,j]

-- and to insert the pivots is simply using the pivot table insert statement,
-- in this case insertArtPersJoinStmt, with the inserter function.

{-- e.g.:
>>> inserter conn insertArtPersJoinStmt (zipWith joinValue pers ixpers)
--}

instance FromRow Pivot where
   fromRow = Pvt <$> field <*> field

instance ToJSON Pivot where
   toJSON (Pvt k1 k2) = object ["article_id" .= k1, "subject_id" .= k2]

{--
This happens frequently: we need to tie together a symbol table to the primary
table. The symbol table is built in a MemoizingTable
--}

buildPivots :: Ord val => Monad m => MemoizingState m [Pivot] Integer val
buildPivots = get >>= \(MT _ keys _, joins) ->
   return (map (uncurry Pvt)
               (concatMap (sequence . (second (map (keys Map.!))))
                          (Map.toList joins)))

{--
Keywords, Authors, and sections. All three follow the same pattern:

* fetch previously stored values
* memoize
* materials values from article set, see which values are new
* store new values, relate all values.

If we are following the same pattern, is there a generalization of the
functions for all of this, so we can have one function that does the work
for any memoize-y type? What would this function look like?
--}

memoizeStore :: ToRow a => Ord a =>
                Connection
             -> (Connection -> IO [IxValue a])
             -> (Connection -> [a] -> IO [Index])
             -> Query
             -> (article -> [a])
             -> [IxValue article]
             -> IO ()
memoizeStore conn fetcher storer pivotQuery getter ixarts =

-- so, 1. we need to fetch currently-stored values and start the MemoizingTable

   fetcher conn >>= \ixvals ->
   let memtable = MT.start (map ix2tup ixvals)
       (ids,arts) = unzip (map ix2tup ixarts)
       stat = execState (zipWithM_ MT.triageM ids (map getter arts))
                                   (memtable,Map.empty)
       substate = Set.toList (MT.newValues (fst stat))
   in  storer conn substate >>= \ixnewvals ->
   let table = MT.update (zip (map idx ixnewvals) substate) (fst stat)
   in  void (executeMany conn pivotQuery
                         (evalState buildPivots (table, snd stat)))

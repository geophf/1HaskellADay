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

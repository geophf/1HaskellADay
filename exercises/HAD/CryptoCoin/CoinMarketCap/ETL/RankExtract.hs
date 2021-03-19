{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.RankExtract where

-- import Control.Monad (forM_)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import qualified Data.ByteString.Char8 as B
{--
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Int (Int64)

import Data.List (partition)

import Data.Map (Map)

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.LookupTable

import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable
import Store.SQL.Connection
--}

import qualified Data.Map as Map

import Data.Time (Day)

import Data.CryptoCurrency.Types
import CryptoCoin.CoinMarketCap.Types

import Store.SQL.Util.Indexed hiding (idx)

data Ranking =
   Ranking { fileId :: Integer, forDay :: Day, 
             coinId :: Integer, place :: Integer }
      deriving (Eq, Ord, Show)

toRank :: IxValue MetaData -> [Ranking]
toRank (IxV i (MetaData (Status d _ _ _ _ _) ecoins)) =
   map (Ranking i d . fi . idx <*> fi . rank) (Map.elems ecoins)
      where fi = fromIntegral

instance ToRow Ranking where
   toRow (Ranking srcId d coinId r) =
      [toField coinId, toField d, toField srcId, toField r]

rankInsertQuery :: Query
rankInsertQuery = Query . B.pack $ unwords [
   "INSERT INTO coin_market_cap_daily_ranking",
   "(cmc_id, date, rank_src_id, rank) VALUES (?, ?, ?, ?)"]

{--
newCoins :: Connection -> MetaData -> IO (Map Idx ECoin)
newCoins conn (MetaData _ m) =
   foldr Map.delete m . map (fromIntegral . idx) <$> coins conn

-- to do that, we need to extract the indices from the database, ... with
-- (any other) value

coins :: Connection -> IO [Index]
coins conn = query_ conn "SELECT cmc_id FROM coin"

-- now that we've got the new coins, we can insert them into our coin-table
-- (and token-table) (and rank-table)

-- to insert into the rank-table, we need a source-id and date.

-- Let's just insert into the coin and token tables? But then, how do we
-- do regression analysis? How do we find what the new coins are? From the
-- dailies? I guess that can work...

instance ToRow CoinInfo where
   toRow (CoinInfo i name symbol slug isActive _rank (Duration f _)) =
      [toField i, toField name, toField symbol, toField isActive,
       toField slug, toField f]

insertCoinInfoQuery :: Query
insertCoinInfoQuery = Query . B.pack $ unwords 
   ["INSERT INTO coin (cmc_id, name, symbol, is_active, slug,",
    "first_historical_data) VALUES (?, ?, ?, ?, ?, ?)"]

instance ToRow Token where
   toRow (Token coininf parentId tok) =
      [toField (i coininf), toField parentId, toField tok]
         where i (CoinInfo i _ _ _ _ _ _) = i

insertTokenQuery :: Query
insertTokenQuery = 
   "INSERT INTO token (token_id, parent_id, token_address) VALUES (?,?,?)"

insertCoin :: Connection -> ECoin -> IO ()
insertCoin conn ecoin =
   insertCoinInfo conn (info ecoin) >> thenInsertCoin conn ecoin

insertCoinInfo :: Connection -> CoinInfo -> IO Int64
insertCoinInfo conn = execute conn insertCoinInfoQuery

thenInsertCoin :: Connection -> ECoin -> IO ()
thenInsertCoin _ (C _) = return ()
thenInsertCoin conn (T tok) = execute conn insertTokenQuery tok >> return ()

-- so, this is how we do it.

-- We insert all the coins first, then we insert the tokens

insertAllCoins :: Connection -> [ECoin] -> IO ()
insertAllCoins conn ecoins =
   let (tokens, coins) = partition isToken ecoins in
   putStrLn ("Inserting " ++ show (length coins) ++ " coins.")   >>
   forM_ coins (insertCoin conn)                                 >>
   putStrLn "...done."                                           >>
   putStrLn ("Inserting " ++ show (length tokens) ++ " tokens.") >>
   forM_ tokens (insertCoin conn)                                >>
   putStrLn "...done."

processOneRankFile :: Connection -> MetaData -> IO ()
processOneRankFile conn md@(MetaData (Status d _ _ _ _ _) ecoins) =
   putStrLn ("\n\nFor ranking file " ++ show d ++ ":") >>
   newCoins conn md                                >>=
   insertAllCoins conn . Map.elems

setProcessed :: Connection -> LookupTable -> IO ()
setProcessed conn srcs =
   execute conn "UPDATE source SET processed=? WHERE source_type_id=?"
           (True, rankingIdx srcs) >>
   putStrLn "Set all ranking files as processed."

-- Process all of them:

go :: IO ()
go =
   withConnection ECOIN (\conn ->
      lookupTable conn "source_type_lk" >>= \srcs ->
      extractRanks conn srcs            >>=
      mapM_ (processOneRankFile conn)   >>
      setProcessed conn srcs)
--}

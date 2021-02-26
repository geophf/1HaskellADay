module CryptoCoin.CoinMarketCap.Ranker where

{--
Hoo, boy! Sparse matrices... Sorry, dense matrices! I remember doing this
stuff with parallel FORTRAN.

We have dates (which are not guaranteed to be consecutive), indices (which are 
also not guaranteed to be consecutive) and rankings (the observed datum).

I'm thinking:

idx,date n, date n-1, date n-2, ..., date 0
1,Just 1, Just 1, ..., Nothing

like that.

This could, I suppose, be represented as a map of maps?
--}

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time

import Data.CryptoCurrency.Types
import CryptoCoin.CoinMarketCap.Types

type RankVector = Map Idx Int
type Matrix = Map Day RankVector

-- TODO: we need a matrix module and a read (that should just be import)
-- and writer function to write out the updated matrix

matrix :: Matrix -> MetaData -> Matrix
matrix m = uncurry ansert . mkRow
   where ansert k v = Map.insert k v m

-- the thing about having keys of Days is that you can't get yesterday
-- with a pred-function. (we can do a Set.maxView, however, for today, then
-- a Set.maxView for yesterday from the remaining keysSet)

-- Okay, so we need to convert a MetaData into a (Day, RankVector), so we can
-- add that row to the matrix

mkRow :: MetaData -> (Day, RankVector)
mkRow (MetaData (Status d _ _ _ _ _) ecoins) =
   (d, Map.fromList $ map (idx &&& rank) ecoins)

yesterday :: Matrix -> Day -> Maybe Day -- but do we even want this?
yesterday = undefined

-- and we need to compute the rank-differential

rankDiff :: Matrix -> RankVector
rankDiff = undefined

-- but to compute rank-diff we need today and yesterday, so, we'll iterate
-- over the map-list. ... Or, ... are we only concerned about today and
-- yesterday?

rankdiff' :: RankVector -> RankVector -> RankVector
rankdiff' today yesterday =
   foldr (inserter yesterday) Map.empty (Map.toList today)

inserter :: RankVector -> (Idx, Int) -> RankVector -> RankVector
inserter yesterday (idx, rank) m =
   maybe m (\r -> Map.insert idx (rank - r) m) (Map.lookup idx yesterday)

{-- which further devolves into an unitary diff

adiff :: Int -> Int -> Int
adiff = (-)

-- and we just lift adiff into the maybe-domain.
--}

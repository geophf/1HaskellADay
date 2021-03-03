{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module CryptoCoin.CoinMarketCap.Analytics.N00Bs where

{--
A thought:

What are the new cryptocurrencies today?

Does it make sense to invest in these new coins?
--}

import Control.Arrow ((***))
import Control.Monad (liftM, join)

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time

import Data.Monetary.Currency
import Data.Monetary.USD
import Data.XHTML (Name)

import Data.CryptoCurrency.Types

import Data.Time.TimeSeries (yesterday, latest)

import CryptoCoin.CoinMarketCap.Types

import CryptoCoin.CoinMarketCap.State.RankMatrix

data N00B = N00B { coin :: ECoin, incept :: Day }
   deriving (Eq, Ord, Show)

noobsFor :: MetaData -> Matrix -> Day -> Set N00B
noobsFor mdata mat day =
   let mbnoobs = liftM (n00bs day mdata) (todayAndYesterday mat day)
   in  fromMaybe Set.empty mbnoobs

todayAndYesterday :: Matrix -> Day -> Maybe (RankVector, RankVector)
todayAndYesterday mat day =
   Map.lookup day mat       >>= \tday ->
   yesterday mat day        >>=
   return . (tday,) . snd

n00bs :: Day -> MetaData -> (RankVector, RankVector) -> Set N00B
n00bs day (MetaData _ coins) (join (***) Map.keysSet -> (tday, yday)) =
   let newsf = map (flip N00B day) . mapMaybe (flip Map.lookup coins)
   in  Set.fromList (newsf $ Set.toList (Set.difference tday yday))

{--
>>> latest rankMatrix 
Just 2021-03-02
>>> let (Just lat) = it
>>> noobsFor rankMatrix lat
{N00B {coin = C (Coin (CoinInfo 7843 "Asia Reserve Currency Coin" 
                                "ARCC" "asia-reserve-currency-coin" True 2864 
                                (Duration {first = 2021-02-28, 
                                           last = 2021-03-02}))), 
       incept = 2021-03-02},
 N00B {coin = C (Coin (CoinInfo 8643 "Shadows" "DOWS" "shadows" True 4219
                                (Duration {first = 2021-03-01, 
                                           last = 2021-03-02}))), 
       incept = 2021-03-02},...}
>>> length it
11

... Okay, we're reporting on these coins. We need to save them with their
incept date and price then check back on them a month (?) later to see how
they've done. TODO.
--}

{-# LANGUAGE TupleSections #-}

module Data.Time.TimeSeries where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time

{--
We represent days as a mapping from Day -> whatever. Given that, what is the
last entry? What is 'yesterday's' entry, relative to some day?
--}

type TimeSeries a = Map Day a

latest :: TimeSeries a -> Maybe Day
latest mat = fst <$> (Set.maxView $ Map.keysSet mat)

yesterday :: TimeSeries a -> Day -> Maybe (Day, a)
yesterday mat tday =
   Set.maxView (Map.keysSet mat) >>= \(day, days) ->
   findYesterday tday day days >>= \yest ->
   Map.lookup yest mat >>= return . (yest,)

findYesterday :: Day -> Day -> Set Day -> Maybe Day
findYesterday tday day days | Set.size days == 0 = Nothing
                            | day < tday         = Just day
                            | otherwise          =
   Set.maxView days >>= uncurry (findYesterday tday)

today :: IO Day
today = utctDay <$> getCurrentTime

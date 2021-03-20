module Data.CryptoCurrency.Types where

import Data.Map (Map)

import Data.Time (Day)
import Data.Time.TimeSeries
import Data.XHTML (Name)

type Idx = Int

class Indexed a where
   idx :: a -> Idx

class Named a where
   namei :: a -> Name

type Symbol = String

class Cymbal a where   -- lol
   sym :: a -> Symbol

class Rank a where
   rank :: a -> Int

class Date a where
   date :: a -> Day

type RankVector = Map Idx Int
type Matrix = TimeSeries RankVector

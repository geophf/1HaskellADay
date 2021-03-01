module Data.CryptoCurrency.Types where

import Data.Map (Map)
import Data.Time

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

type RankVector = Map Idx Int
type Matrix = Map Day RankVector

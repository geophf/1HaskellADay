module Data.CryptoCurrency.Types where

type Idx = Int

class Indexed a where
   idx :: a -> Idx

type Name = String

class Named a where
   namei :: a -> Name

type Symbol = String

class Cymbal a where   -- lol
   sym :: a -> Symbol

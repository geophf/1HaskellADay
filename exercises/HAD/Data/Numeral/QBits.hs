module Data.Numeral.QBits where

-- Represents a specific view as digits-as-qbits.

import Data.QBit

type Digit = QBit Int

data Nums = N [Digit]

instance Show Nums where
   show (N digs) = concatMap show digs

asNum :: Nums -> Int
asNum (N digs) = n 0 (reverse digs)
   where n _ [] = 0
         n p (h:t) = 10 ^ p * extract h + n (succ p) t

-- for constraining values of QBits, see eitherOr, neitherNor, andNot

-- For digital constraints we have:

notZero :: Integral a => a -> Bool
notZero = (/= 0)

iseven, isodd :: Integral a => a -> Bool
iseven = (== 0) . (`mod` 2)
isodd = not . iseven

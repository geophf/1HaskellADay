module Data.Monetary.USD where

-- Representation of US dollars as rational numbers
-- 2015-12-07: Corrected read parse errors for stupid (AAPL) numbers like: $.12

import Data.Monoid

import Control.Presentation
import Data.Monetary.Currency

-- Spraken dollars, mang!

data USD = USD Rational deriving Eq

-- one line of declaration followed by HOW many lines of instantiation?

instance Ord USD where USD x <= USD y = x <= y
instance Show USD where show (USD x) = '$':laxmi 2 x
instance Read USD where readsPrec _ ('$':val) = [(mknMoney USD ('0':val), "")]

-- *Main> read "$.12" :: USD ~> $0.12
-- *Main> read "$1.12" :: USD ~> $1.12
-- *Main> read "$0.12" :: USD ~> $0.12 
-- okay, how hard was that AAPL? 
-- e.g.: http://investor.apple.com/dividends.cfm

instance Raw USD where rep (USD x) = laxmi 2 x

instance Currency USD where
   value (USD x) = x     -- so Price-types are copointed ...

instance Num USD where
   d1 - d2 = USD $ value d1 - value d2
   negate dollar = USD $ 0.0 - value dollar
   d1 + d2 = USD $ value d1 + value d2
   d1 * d2 = USD $ value d1 * value d2
   abs dollar = USD $ abs (value dollar)
   signum dollar = USD $ signum (value dollar)
   fromInteger x = USD (fromInteger x)

instance Fractional USD where
   d1 / d2 = USD $ value d1 / value d2
   fromRational = USD

instance Monoid USD where
   mempty = USD 0
   (USD a) `mappend` (USD b) = USD $ a + b
-- because when you mappend dollars they are summed.

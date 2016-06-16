{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Control.Presentation where

-- some presentation-layer stuff, existentially

class Univ a where explode :: a -> [String]

shower :: forall a. forall b. Show a => [b -> a] -> b -> [a]
shower = flip $ map . flip ($)
   -- #1liner challenge answered by @gautier_difolco

-- alternatively, just show me the raw value within the type
-- GIMME BYTES, BAYBEE!!!11!1!!!

class Raw r where rep :: r -> String

-- Yeah, I'm going there! Shower allows you to create uniform showable
-- types for construction of CSV-rows

data Shower = forall a. Show a => S a | forall b. Raw b => Sr b

instance Show Shower where
   show (S a) = show a
   show (Sr b) = rep b

-- converts a Rational to a stringified-n-digit representation

laxmi :: Int -> Rational -> String
laxmi digs x = let dollars = floor x
                   expon   = 10 ^ digs
                   cents   = floor (x * fromIntegral expon) - dollars * expon
          in  show dollars ++ ('.':showAllDa digs (show cents))
              where showAllDa n x = replicate (n - length x) '0' ++ x

{--
*Analytics.Trading.Data.Strategy> USD 10.01 ~> $10.1 ... uh, oh!

... geophf puts in fix, then:

*Analytics.Trading.Data.Strategy> USD 10.01 ~> $10.01
*Analytics.Trading.Data.Strategy> USD 10.1 ~> $10.10
*Analytics.Trading.Data.Strategy> USD 10.012134 ~> $10.01

WHEW!
--}

{--
An example definition of a Univ-type using shower is:

instance Univ Analysis where
   explode = map show . shower [S . dayo, Sr . closo, Sr . rato]
--}

module Data.QBit where

import Control.Arrow (first)
import Control.Monad ((>=>))
import Data.List (delete)

-- below imports available via 1HaskellADay git repository

import Control.List (takeout)
import Control.Logic.Frege (assert)

{--
I use this all the time when I'm doing logic programming in functional
languages.

I suppose I could've called this thing an attributed variable or a constraint-
logic variable, but I know enough of neither of those to speak here with
any authority.

Now, QBits ... I have done a bit of the math, and it's HARD! But the
QBits themselves:

| 1 > + | 0 > = something or other

are pretty darn neat!

So here it is. And I do love me my logic variables, and these allow one to
build in constraints, so that makes them so much the better.

--}

-- a QBit is in an indetermined state, it's either ground to a letter
-- (fer shur) or it's possibly one of an exhaustible pool of letters

data QBit a = SuperPosition ([a] -> [(a, [a])]) | Observed a

instance Show a => Show (QBit a) where
   show (SuperPosition _) = "__" -- 2 characters .. yeah, to make 'tail' work
   show (Observed a) = show a

observed :: QBit a -> Bool
observed (Observed _) = True
observed _            = False

-- a sample implementation: 

free :: Eq a => QBit a
free = SuperPosition takeout

-- We can also draw with an inlined constraint:

constrain :: Eq a => (a -> Bool) -> QBit a
constrain fn = SuperPosition (takeout >=> assert (fn . fst))

-- draws a value from the pool of available values and settles the QBit
-- to that value yielding it and the remaining available values.

-- An alternative is a draw-from without disturbing the pool, but that
-- is already (too) well-understood, so it's not needed here.

draw :: Eq a => QBit a -> [a] -> [(QBit a, [a])]
draw (SuperPosition f) pool = first Observed <$> f pool
draw obs@(Observed a) pool = [(obs, delete a pool)]

-- *Data.QBit Control.List> draw free [1..5] ~>
-- [(1,[2,3,4,5]),(2,[1,3,4,5]),(3,[1,2,4,5]),(4,[1,2,3,5]),(5,[1,2,3,4])]

-- and the listy version of draw:

draws :: Eq a => [QBit a] -> [a] -> [([QBit a], [a])]
draws [] sommat = -- erhm ... idk
   [([], sommat)] -- okay, that worked! :D
draws (b:its) pool = draw b pool >>= \(b1, p1) ->
   first (b1:) <$> draws its p1
 
-- *Data.QBit Control.List> draws (replicate 2 free) [1..3] ~>
-- [([1,2],[3]),([1,3],[2]),([2,1],[3]),([2,3],[1]),([3,1],[2]),([3,2],[1])]

class Copointed b where
   extract :: b a -> a

instance Copointed QBit where
   extract (Observed x) = x
   extract _ = error "Cannot extract information from a SuperPosition"

-- how manipulate (observed) QBits as numbers:

-- first the odiously obligatory Eq-constraint:

instance Eq a => Eq (QBit a) where
   SuperPosition _ == _               = False
   _               == SuperPosition _ = False
   Observed a      == Observed b      = a == b
   SuperPosition _ /= _               = False
   _               /= SuperPosition _ = False
   Observed a      /= Observed b      = a /= b

-- ... so that we can make our QBits over numbers BE numbers.

instance Num a => Num (QBit a) where
   q1 + q2     = Observed (sum (map extract [q1, q2]))
   q1 * q2     = Observed (product (map extract [q1, q2]))
   abs         = Observed . abs . extract
   signum      = Observed . signum . extract
   fromInteger = Observed . fromInteger
   negate      = Observed . negate . extract

-- *Data.QBit> Observed 3 + Observed 4 ~> (Observed) 7
-- *Data.QBit> Observed 3 + free ~>
-- *** Exception: Cannot extract information from a SuperPosition

-- Constrains values of QBits for Eq a values:

eitherOr, neitherNor :: Int -> Int -> (Int -> Bool)
eitherOr a b = (||) . (== a) <*> (== b)
neitherNor n m = (&&) . (/= n) <*> (/= m)

andNot :: Int -> (Int -> Bool)
andNot = (/=)

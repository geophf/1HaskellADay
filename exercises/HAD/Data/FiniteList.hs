module Data.FiniteList where

{--
Okay, I got bored counting the length of finite lists in Haskell. I mean,
FORTH had the right idea ... in 1970: Strings (of any type of element) should
have the length embedded, so lengthV is an O(1) operation

SHEESH!
--}

import Data.Maybe (fromMaybe)

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)

data Vect a = Vec { len :: Int, vec :: [a] } deriving (Eq, Show)

-- our base case:

nullV :: Vect a
nullV = Vec 0 []

-- our cons(tructor)

consV :: a -> Vect a -> Vect a
consV h (Vec n t) = Vec (succ n) (h:t)

{--
>>> consV 1 nullV
Vec {len = 1, vec = [1]}

>>> consV 's' (list2Vect "mart")
Vec {len = 5, vec = "smart"}

... Smart. Maxwell Smart.
--}

-- and our fromList constructor

list2Vect :: [a] -> Vect a
list2Vect = Vec . length <*> id

{--
>>> list2Vect "hello, world!"
Vec {len = 13, vec = "hello, world!"}
--}

-- and we need our coinductive functions on vects:

dropV :: Int -> Vect a -> Vect a
dropV n (Vec m list) = Vec (max 0 (m - n)) (drop n list)

{--
>>> dropV 2 (list2Vect [1..10])
Vec {len = 8, vec = [3,4,5,6,7,8,9,10]}

>>> dropV 10 (list2Vect "abc")
Vec {len = 0, vec = ""}
--}

-- safe head-function on vectors

splitV :: Vect a -> Maybe (a, Vect a)
splitV (Vec 0 []) = Nothing
splitV (Vec n (h:t)) = Just (h, Vec (pred n) t)

{--
>>> splitV (Vec 5 [1..5])
Just (1,Vec {len = 4, vec = [2,3,4,5]})

>>> splitV nullV
Nothing
--}

dropUntilV :: (a -> Bool) -> Vect a -> Vect a
dropUntilV f vect =
   fromMaybe nullV (splitV vect >>= \(h, t) -> 
                    return (if f h then vect else dropUntilV f t))

{--
>>> dropUntilV (== 'q') (list2Vect ['a' .. 'z'])
Vec {len = 10, vec = "qrstuvwxyz"}

... but if we don't want to include the 'q' in the result:

>>> dropUntilV (> 'q') (list2Vect ['a' .. 'z'])
Vec {len = 9, vec = "rstuvwxyz"}

... and here we demonstrate dropUntilV as a noop:

>>> dropUntilV (/= 'q') (list2Vect ['a' .. 'z'])
Vec {len = 26, vec = "abcdefghijklmnopqrstuvwxyz"}

... and here is Zurg, destroyer of worlds:

>>> dropUntilV (== 'q') (list2Vect ['a' .. 'm'])
Vec {len = 0, vec = ""}
--}

-- Now, is Vect a a functor, foldable, traversable, monadic? 
-- erhm... YES! OF COURSE! ... when I care.

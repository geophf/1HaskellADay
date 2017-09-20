module Data.Bag where

{-- We "re"present the Bag data-type. The 'Bag' or 'MultiSet' data type. 'Bag' 
is from anon. 'MultiSet' is a nonesensical word from more recent times from 
those too embarrassed to used the word 'bag' in a sentence, so they say the word
'multiset,' which means a set of unique objects with a count for each unique
object that is ... repeated ... in the set.

You see the absurdity? A set contains unique objects, but a multi-set contains
multiple unique objects. So are these unique objects unique? No, of course not,
as there are multiples of them, so it's either not a set (because the objects
are not unique) or the objects are not unique in the set (which, by definition,
contains only unique objects).

... 'MultiSet.' Yeah.

Whatever.

But we CAN'T say the word 'bag' because that's derogatory somehow, so, instead,
we say an absurdity ('MultiSet') because we don't want to look like fools
saying the word 'bag.'

We just want to say absurdities, but give them multi-syllabic renderings, so we
look sophisticated saying these absurdities.
--}

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Ord

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))

-- an implementation of the bag data type from http://lpaste.net/107881

{-- 

A bag is a collection of elements grouped and counted by isomorphism.
So if you had:

*Data.Bag> Bag.fromList [1,2,2,1,1,1,3] 

The bag would have something like the following representation:

~> Bag { (1, 4), (2, 2), (3, 1) }

 --}

type Bag a = Map a (Sum Int)

emptyBag :: Bag a
emptyBag = Map.empty

addn :: Ord a => a -> Int -> Bag a -> Bag a
addn elt = Map.insertWith mappend elt . Sum

add :: Ord a => a -> Bag a -> Bag a
add = flip addn 1  -- geddit? 'add a bag'? geddit?
-- ... I don't get it.

-- the above is from my blog entry on the bag data type in Idris at
-- http://logicaltypes.blogspot.com/2014/06/thats-totes-my-bag.html

-- since Bag IS(actually)A Map, all the instantiation follows.

-- This happens often enough; we want to rank by the frequency, most frequent
-- first

rank :: Ord a => Bag a -> [(a, Int)]
rank = sortOn (Down . snd) . toList

asMap :: Ord a => Bag a -> Map a Int
asMap = Map.map getSum

toList :: Ord a => Bag a -> [(a, Int)]
toList = Map.toList . asMap

-- we used to have a countOf, but now this reduces, simply, to Map.!

-- ... but we do need fromList still

fromList :: Ord a => [a] -> Bag a
fromList = foldr add emptyBag

size :: Bag a -> Int
size = getSum . sum . Map.elems

merge :: Ord a => Bag a -> Bag a -> Bag a
merge =

-- well, let's think about this merge a and b is

-- get all the elements in a that are not in b and vice versa:
-- now we need to sum the sums of the a /\ b

-- this is Map.mergeWithKey, yes?
   Map.mergeWithKey (const (pure <<- (<>))) id id

module Y2017.M02.D07.Exercise where

import Control.Comonad
import Data.Set (Set, fromList)

{--
Rosalind problem solving, day 2, from: http://rosalind.info/problems/seto/

Introduction to Set Operations solved by 1182, as of February 6th, 2017

Forming New Sets

Just as numbers can be added, subtracted, and multiplied, we can manipulate sets
in certain basic ways. The natural operations on sets are to combine their 
elements, to find those elements common to both sets, and to determine which 
elements belong to one set but not another.

Just as graph theory is the mathematical study of graphs and their properties, 
set theory is the mathematical study of sets and their properties.

Problem

If A and B are sets, then their union A∪B is the set comprising any elements 
in either A or B; their intersection A∩B is the set of elements in both A and B;
and their set difference A−B is the set of elements in A but not in B.

Furthermore, if A is a subset of another set U, then the set complement of A
with respect to U is defined as the set Ac=U−A. See the Sample sections below 
for examples.

Given: A positive integer n, n <= 20,000, and two subsets A and B of {1,2,...,n}

Return: Six sets: A∪B, A∩B, A−B, B−A, Ac, and Bc
(where set complements are taken with respect to {1,2,...,n}).
--}

sample :: String
sample = unlines ["10", "{1, 2, 3, 4, 5}", "{2, 8, 5, 10}"]

{--

or: 

10
{1, 2, 3, 4, 5}
{2, 8, 5, 10}

When the above operations are run, you will get the following:
--}

result :: [Grp Int]
result = map (G . fromList)
             [[1,2,3,4,5,8,10],[2,5],[1,3,4],[8,10],[6..10],[1,3,4,6,7,9]]

{--
or, visually:

{1, 2, 3, 4, 5, 8, 10}
{2, 5}
{1, 3, 4}
{8, 10}
{8, 9, 10, 6, 7}
{1, 3, 4, 6, 7, 9}

Extra Information

From the definitions above, one can see that A∪B=B∪A and A∩B=B∩A for all sets A
and B, but it is not necessarily the case that A−B=B−A (as seen in the Sample 
sections above). This set theoretical fact parallels the arithmetical fact that 
addition is commutative but subtraction is not.

--}

-- Okay, to read the sample input and write the sample output, we need a
-- type value:

data Grp a = G (Set a)
   deriving (Eq, Ord)

instance Show a => Show (Grp a) where
   show grp = undefined

instance Read a => Read (Grp a) where
   readsPrec n = undefined

-- So that a Grp, such as {1, 2, 3, 4, 5} and be read and shown as such

-- Now we need the set theory operations on Grps. Hint: look at Data.Set

(∪), (∩), (−) :: Ord a => Grp a -> Grp a -> Grp a
a ∪ b = undefined
a ∩ b = undefined
a − b = undefined

-- Now (a complement) is interesting because is it an unary operation that
-- entirely depends on what the U, or universe, is. Another way of putting
-- it is that complement is comonadic. So we well defined it that way

-- This also means that Grp is a comonad. So we must make it comonadic.

instance Comonad Grp where
   extract grp  = undefined
   extend f grp = undefined

-- of course, for Grp to be a comonad, it has to be a functor, so:

instance Functor Grp where
   fmap f grp = undefined

-- Now that Grp is a Comonad, we can make complement comonadic

complement :: Comonad w => w a -> Grp a -> Grp a
complement context a = undefined

-- So, here's the kicker: the comonad isn't any old set, it is the universe,
-- (in other words, the 'context') in which the set exists to find its 
-- complement. GEDDIT? ... well, give that a try. If that doesn't work for you
-- define complement in such a way that works for you best.

-- It's still a comonad, but okay.

-- Now that we have the above group operations defined, we tie it all together:

-- 1. read in the context (the size of the universe) and the two sets
-- 2. output each computation, one for each line

grouper :: String -> IO [Grp Int]
grouper input = undefined

-- verify that the input, shown above when run through the program grouper,
-- properly prints the output, yes, but also equals the groups enumerated.

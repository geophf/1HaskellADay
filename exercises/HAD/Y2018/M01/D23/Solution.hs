module Y2018.M01.D23.Solution where

{--
Continuing on our Prolog-y adventures in Haskell, let's look at P27 from P99:

P27 (**) Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 
3 and 4 persons? Write a predicate that generates all the possibilities via 
backtracking.

Example:
?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
...

That is to say (in Haskell terms), generate from a list of 9 elements all the
combinations of 2, 3, and 4 elements.
--}

import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Control.List (takeout)
import Data.QBit

group3 :: Ord a => [a] -> Set [Set a]
group3 elts = 

-- we can do this with list compression? But we have to avoid pulling the same
-- value twice. So, let's order takeout:

   Set.fromList (takeout elts >>= \(e1, elts1) -> -- ... etc
   takeout elts1 >>= \(e2, elts2) ->
   takeout elts2 >>= \(e3, elts3) ->
   takeout elts3 >>= \(e4, elts4) ->
   takeout elts4 >>= \(e5, elts5) ->
   takeout elts5 >>= \(e6, elts6) ->
   takeout elts6 >>= \(e7, elts7) ->
   takeout elts7 >>= \(e8, elts8) ->
   takeout elts8 >>= \(e9, []) ->
   return (map Set.fromList [[e1,e2],[e3,e4,e5],[e6,e7,e8,e9]]))

-- you can also do this with careful permutation

-- given:

data Peeps = Aldo | Beat | Carla | David | Evi | Flip | Gary | Hugo | Ida
   deriving (Eq, Ord, Enum, Show)

{--
>>> take 5 $ Set.toList (group3 [Aldo .. Ida])
[[fromList [Aldo,Beat],fromList [Carla,David,Evi],fromList [Flip,Gary,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,David,Flip],fromList [Evi,Gary,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,David,Gary],fromList [Evi,Flip,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,David,Hugo],fromList [Evi,Flip,Gary,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,David,Ida],fromList [Evi,Flip,Gary,Hugo]]]

>>> length $ group3 [Aldo .. Ida]
1260

That took awhile. Of course, there are combinators that can be used to compute
these numbers instead of counting every distinct set ...
--}

{-- BONUS -----------------------------------------------------------------

b) Generalize the above predicate in a way that we can specify a list of group 
sizes and the predicate will return a list of groups.

Example:
?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
...

Note that we do not want permutations of the group members; i.e. 
[[aldo,beat],...] is the same solution as [[beat,aldo],...]. However, we make a 
difference between [[aldo,beat],[carla,david],...] and 
[[carla,david],[aldo,beat],...].

You may find more about this combinatorial problem in a good book on discrete 
mathematics under the term "multinomial coefficients".
--}

grouper :: Ord a => [a] -> [Int] -> Set [Set a]
grouper peeps sizes =
   let bits = map (`replicate` free) sizes
       soln = drawing bits peeps
   in  Set.fromList (map (map (Set.fromList . map extract)) soln)

drawing :: Ord a => [[QBit a]] -> [a] -> [[[QBit a]]]
drawing [] _ = [[]]
drawing (q:q') list = draws q list >>= \(qs, rest) ->
   drawing q' rest >>= return . (qs:)

{--
>>> take 5 $ Set.toList (grouper [Aldo .. Ida] [2,2,5])
[[fromList [Aldo,Beat],fromList [Carla,David],fromList [Evi,Flip,Gary,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,Evi],fromList [David,Flip,Gary,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,Flip],fromList [David,Evi,Gary,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,Gary],fromList [David,Evi,Flip,Hugo,Ida]],
 [fromList [Aldo,Beat],fromList [Carla,Hugo],fromList [David,Evi,Flip,Gary,Ida]]]

>>> length $ grouper [Aldo .. Ida] [2,2,5]
756
--}

-- of course, I had invented takeouts centuries ago ... but only as QBits, smh

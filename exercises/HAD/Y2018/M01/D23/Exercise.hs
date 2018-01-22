module Y2018.M01.D23.Exercise where

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

group3 :: Ord a => [a] -> Set (Set a)
group3 elts = undefined

-- given:

data Peeps = Aldo | Beat | Carla | David | Evi | Flip | Gary | Hugo | Ida
   deriving (Eq, Ord, Show)

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

grouper :: Ord a => [a] -> [Int] -> Set (Set a)
grouper peeps sizes = undefined

module Y2018.M02.D21.Solution where

{--
More P99 fun with lists from:

http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

P28 (**) Sorting a list of lists according to length of sublists
a) We suppose that a list (InList) contains elements that are lists themselves. 
The objective is to sort the elements of InList according to their length. E.g. 
short lists first, longer lists later, or vice versa.

Example:
?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]
--}

import Control.Arrow ((&&&))

import Data.List (sortOn, groupBy)
import Data.Function (on)

lsort :: [[a]] -> [[a]]
lsort = sortOn length  -- from Denis Stoyanov @xgrommx

-- create a list of lists by matching the lengths against the ints
-- until the ints or lengths is exhausted:

lengths, ints :: [Int]
lengths = [1,8,7,4,9,5,4,1,2,7,
           7,6,4,9,4,5,6,5,5,5,
           6,4,6,4,3,6,1,7,3,8,
           1,8,2,6,1,1,3,5,8,7,
           9,2,6,6,8,7,8,2,5,5]

ints = [33,61,14,26,50,22,87,61,98,24,
        8,71,24,89,44,3,42,21,16,62,
        8,94,82,57,59,18,50,30,54,47,
        64,35,88,36,81,40,48,62,61,81,
        31,37,13,99,35,29,33,94,81,29,
        39,62,11,18,48,45,33,43,99,49,
        17,49,21,44,33,16,33,48,73,35,
        81,47,96,54,23,62,6,94,16,44,
        95,4,76,14,71,2,4,70,22,13,
        9,96,9,55,12,91,63,41,33,37]

-- random numbers provided by random.org

mklistolists :: [Int] -> [a] -> [[a]]
mklistolists [] _ = []
mklistolists _ [] = []
mklistolists (len:gths) list@(_:_) = let (subl,ist) = splitAt len list in
   subl:mklistolists gths ist

-- what is your listolists? What is your lfsort listolists?

{--
>>> listies = mklistolists lengths ints
>>> length listies
20
>>> take 4 listies
[[33],[61,14,26,50,22,87,61,98],[24,8,71,24,89,44,3],[42,21,16,62]]

>>> lsort listies 
[[33],[61],[37],[81,31],[42,21,16,62],[81,40,48,62],[49,21,44,33],[23,62,6,94],
 [47,64,35,88,36],[16,44,95,4,76],[13,9,96,9,55],[12,91,63,41,33],
 [45,33,43,99,49,17],[14,71,2,4,70,22],[24,8,71,24,89,44,3],
 [37,13,99,35,29,33,94],[81,29,39,62,11,18,48],[61,14,26,50,22,87,61,98],
 [8,94,82,57,59,18,50,30,54],[16,33,48,73,35,81,47,96,54]]
--}

{-- BONUS -----------------------------------------------------------------

b) Again, we suppose that a list (InList) contains elements that are lists 
themselves. But this time the objective is to sort the elements of InList 
according to their length frequency; i.e. in the default, where sorting is done 
ascendingly, lists with rare lengths are placed first, others with a more 
frequent length come later.

Example:
?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]

Note that in the above example, the first two lists in the result L have length 
4 and 1, both lengths appear just once. The third and forth list have length 3 
which appears, there are two list of this length. And finally, the last three 
lists have length 2. This is the most frequent length.
--}

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort

{--
or from Bazzargh @bazzargh
 
let {g=flip on length;f=sortBy (g compare)} in (concat . f .  groupBy (g (==)) . f)

>>> lfsort (mklistolists lengths ints)
[[81,31],[61,14,26,50,22,87,61,98],[45,33,43,99,49,17],[14,71,2,4,70,22],
 [8,94,82,57,59,18,50,30,54],[16,33,48,73,35,81,47,96,54],[33],[61],[37],
 [24,8,71,24,89,44,3],[37,13,99,35,29,33,94],[81,29,39,62,11,18,48],
 [42,21,16,62],[81,40,48,62],[49,21,44,33],[23,62,6,94],[47,64,35,88,36],
 [16,44,95,4,76],[13,9,96,9,55],[12,91,63,41,33]]

And there we go!
--}

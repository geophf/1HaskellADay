module Control.List where

import Control.Applicative (liftA2)
import Control.Arrow ((***), (&&&))
import Control.Comonad
import Control.Monad

import Data.Foldable (toList)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Okay, list not being a comonad is just wrong. End of discussion

instance Comonad [] where
   extract = head
   duplicate [] = []
   duplicate list@(h:t) = list : duplicate t
   -- or duplicate = init . tails for nonempty lists

-- I'm always testing for lists of only one element, so ...

singleton :: [a] -> Bool
singleton = (== 1) . length

-- (the whole finite set of dependent types in Haskell thing ... *blush*)

-- A set of operations not included in the standard library of Data.List

-- chops, as in mutton. We chop a list into little, tiny pieces, and then
-- eat them up, yum!

chops :: Int -> [a] -> [[a]]
chops _ [] = []
chops n list = take n list : chops n (drop n list)

-- I could have done a fold there, but ... eh.

-- *Main> chops 4 [1..12] ~> [[1,2,3,4],[5,6,7,8],[9,10,11,12]]

-- softtail, because Harley Davidson

softtail :: [a] -> [a]
softtail [] = []
softtail (h:t) = t

-- powerSet, because we all need a little powerSet-love-action now and again

-- declarative semantics, but blows up
powerSet :: Ord a => [a] -> [[a]]
powerSet lst = lst : (Set.toList (Set.fromList (concat (ps lst))))

ps :: Ord a => [a] -> [[[a]]]
ps [] = return [[]]
ps lst@(_:_) = takeout lst >>= \(_, rest) -> ps rest >>= return . (:) rest

{-- much, Much, MUCH more efficient implementation ...
powerSet :: Ord a => [a] -> [[a]]
powerSet list =
   let len = length list
   in  [len, pred len .. 1] >>= flip choose list

-- *Control.List> powerSet [1,2,3] ~> [[1,2,3],[1,2],[1,3],[2,3],[1],[2],[3]]
--}

-- takeout grabs an element from a list and returns it and the rest of the
-- list

takeout :: Eq a => [a] -> [(a, [a])]
takeout [] = mzero
takeout list@(_:_) = [ (elt, rest) | elt <- list, let rest = delete elt list ]

-- takeout is very 'State-y'

-- permute

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
{--

Explanation for permute [] = [[]] (n.b.: NOT permute [] = []) is at
http://stackoverflow.com/questions/6627721/recursive-permutation-function-always-returns-empty-list

Side note: the number of permutations for list of length n is n!
 --}

permute list = -- [h:t | (h, rest) <- takeout list, t <- permute rest]
   takeout list >>= uncurry map . ((:) *** permute)

-- to compute _A_ permutation in linear time (instead of computing all
-- permutations in O(N*N!) time), we do the following:

{-- where is Random and System.Random???
perm :: [a] -> IO [a]
perm list@(_:_) = p' list (pred $ length list)
   where p' list 1 = return list
         p' list@(h:t) n = getStdRandom (randomR (0,n)) >>= \idx ->
            let (pre, s, post) = select idx list
            in  p' (wtail pre h ++ post) (pred n) >>= return . (s :)
--}

-- wtail consumes an element, even if it's from the next list

wtail :: [a] -> a -> [a]
wtail [] h = []
wtail (h:t) x = t ++ [x]

-- select separates a list to (pre, selected element, post)
select :: Int -> [a] -> ([a], a, [a])
select 0 (h:t) = ([], h, t)
select n list = let (h:t) = drop n list in (take n list, h, t)

-- choose chooses choosily n elements of list giving all combinations of same

{--
choose :: (Enum a, Ord a) => Int -> [a] -> [[a]]
choose _ [] = []
choose n list@(_:_) = c' (fromInt n) (toEnum 0) [] list
   where c' Z _ accum _ = return $ reverse accum
         c' (S n) min accum seed = takeout seed >>= \(a, s) ->
              guard (a > min) >> c' n a (a : accum) s

-- let's give an efficient chooser for greater k :/
 --}
choose :: Ord a => Int -> [a] -> [[a]]
choose k list =
   let n = length list
       halfn = div n 2
   in  (if k <= halfn then buildupChoose k else removeFromChoose (n - k)) list

removeFromChoose :: Ord a => Int -> [a] -> [[a]]
removeFromChoose 0 list = return list
removeFromChoose nk list = 
   map Set.toList (Set.toList (Set.fromList 
   (map Set.fromList (takeout list >>= \(_, rest) ->
   removeFromChoose (pred nk) rest))))

buildupChoose :: Ord a => Int -> [a] -> [[a]]
buildupChoose 0 _ = return []
buildupChoose n list = 
   map Set.toList (Set.toList (Set.fromList (takeout list >>= \(elt, rest) ->
   buildupChoose (pred n) rest >>= return . Set.fromList . (:) elt)))

-- if we wish to know our commitment of nCk we can precompute its length:

lenChoose :: Integer -> Integer -> Integer
lenChoose n k = fac n `div` (fac k * fac (n-k))
   where fac x = product [1..x]

{--
-- determines that a list of lists have all unique elements
-- (this has come up more than once)

allUniqElems :: Eq a => [[a]] -> Bool
allUniqElems [] = True
allUniqElems ((e:es):lists) = 
   a' e es && a' e (join lists) && allUniqElems lists
      where a' element list = element `notElem` list

-- commented out; this implementation isn't quite right! :/

 --}

-- consList proves list has (at least one) element(s)

consList :: [a] -> Bool
consList (_:_) = True
consList _     = False

-- take the number of rows request, OR, if we cannot, then just give
-- the first row as an answer (the default value).

mbtake :: (Enum n, Num n, Eq n) => [a] -> n -> Either [a] a
mbtake (h:t) x = takeThis t (pred x) [h] -- good thing order is unimportant!
   where takeThis _     0 ans = Left ans
         takeThis []    x _   = Right h
         takeThis (h:t) x ans = takeThis t (pred x) (h:ans)

minmax :: (Foldable t, Ord a) => t a -> (a, a)
minmax = uncurry (foldr (liftA2 (***) min max))
               . ((head &&& head) &&& tail) . toList
   -- obadz @obadzz solution to #1Liner 2016-04-13

weave :: [String] -> String
weave = intercalate ","

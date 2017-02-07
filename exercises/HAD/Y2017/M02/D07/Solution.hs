module Y2017.M02.D07.Solution where

import Control.Arrow ((&&&))
import Control.Comonad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

import Codec.Compression.GZip

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-), adjoin)

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

readSample :: String -> (Int, [Grp Int])
readSample = (read . head &&& map read . tail) . lines

{--
or: 

10
{1, 2, 3, 4, 5}
{2, 8, 5, 10}

*Y2017.M02.D07.Solution> let (uni, [a,b]) = readSample sample ~>
(10,[{1, 2, 3, 4, 5},{2, 5, 8, 10}])

When the above operations are run, you will get the following:
--}

result :: [Grp Int]
result = map (G . Set.fromList)
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

data Grp a = G { grp :: Set a }
   deriving (Eq, Ord)

instance Show a => Show (Grp a) where
   show = ('{':) . (++ "}") . intercalate ", " . map show . Set.toList . grp

-- *Y2017.M02.D07.Solution> G (fromList [1..5]) ~> {1, 2, 3, 4, 5}

instance (Ord a, Read a) => Read (Grp a) where
   readsPrec n ('{':list) = [(G . Set.fromList $ read ('[':init list ++ "]"), "")]

{--
*Y2017.M02.D07.Solution> (read "{ 1,4, 6, 9, 10}") :: Grp Int ~> {1, 4, 6, 9, 10}
--}
-- So that a Grp, such as {1, 2, 3, 4, 5} and be read and shown as such

-- Now we need the set theory operations on Grps. Hint: look at Data.Set

(∪), (∩), (−) :: Ord a => Grp a -> Grp a -> Grp a

-- (∪) = G . uncurry union <<- curry (adjoin grp) -- or, put another way:
(∪) = setop Set.union

-- *Y2017.M02.D07.Solution> a ∪ b ~> {1, 2, 3, 4, 5, 8, 10}

(∩) = setop Set.intersection

-- *Y2017.M02.D07.Solution> a ∩ b ~> {2, 5}

(−) = setop Set.difference

-- *Y2017.M02.D07.Solution> a − b ~> {1, 3, 4}

setop :: Ord a => (Set a -> Set a -> Set a) -> Grp a -> Grp a -> Grp a
setop op = G . uncurry op <<- curry (adjoin grp)

-- Now (a complement) is interesting because is it an unary operation that
-- entirely depends on what the U, or universe, is. Another way of putting
-- it is that complement is comonadic. So we well defined it that way

-- This also means that Grp is a comonad. So we must make it comonadic.

{--
instance Comonad Grp where
   extract = (\[_, s, _] -> head $ Set.toList s) . Set.splitRoot . grp
   extend f = undefined -- G . Set.map f . grp

-- of course, for Grp to be a comonad, it has to be a functor, so:

instance Functor Grp where
   fmap f = G . Set.map f . grp

-- Now that Grp is a Comonad, we can make complement comonadic

complement :: Comonad w => w a -> Grp a -> Grp a
complement context a = undefined

Of course, for Grp to be a Functor, Set has to be, as well, and Set is not
a Functor. Hm. Another day, then.

-- So, here's the kicker: the comonad isn't any old set, it is the universe,
-- (in other words, the 'context') in which the set exists to find its 
-- complement. GEDDIT? ... well, give that a try. If that doesn't work for you
-- define complement in such a way that works for you best.

-- It's still a comonad, but okay.
--}

-- So I proposed the above for complement to push my agenda: comonads
-- (I LOVE'm!). Another way to look at complement is that it is a function
-- with the universe value curried in. So, we can have complement declared:

complemnt :: Ord a => Grp a -> Grp a -> Grp a
complemnt universe a = universe − a

-- if that is a better frame with which to define that function for you.

-- Of course, we need to define how we get this universe, then, don't we.

universe :: Int -> Grp Int
universe n = G (Set.fromList [1..n])

{--
*Y2017.M02.D07.Solution> let univ = universe uni
*Y2017.M02.D07.Solution> complemnt univ a ~> {6, 7, 8, 9, 10}
*Y2017.M02.D07.Solution> complemnt univ b ~> {1, 3, 4, 6, 7, 9}
--}

-- Now that we have the above group operations defined, we tie it all together:

-- 1. read in the context (the size of the universe) and the two sets
-- 2. output each computation, one for each line

grouper :: String -> IO [Grp Int]
grouper input = 
   let (uni, [a,b]) = readSample input
       grp1 = a ∪ b
       grp2 = a ∩ b
       grp3 = a − b
       grp4 = b − a
       univ = universe uni
       grp5 = complemnt univ a
       grp6 = complemnt univ b
       ans = [grp1, grp2, grp3, grp4, grp5, grp6]
   in  mapM_ print ans >> return ans

-- verify that the input, shown above when run through the program grouper,
-- properly prints the output, yes, but also equals the groups enumerated.

{--
*Y2017.M02.D07.Solution> grouper sample 
{1, 2, 3, 4, 5, 8, 10}
{2, 5}
{1, 3, 4}
{8, 10}
{6, 7, 8, 9, 10}
{1, 3, 4, 6, 7, 9}
[{1, 2, 3, 4, 5, 8, 10},{2, 5},{1, 3, 4},{8, 10},{6, 7, 8, 9, 10},{1, 3, 4, 6, 7, 9}]
*Y2017.M02.D07.Solution> result == it ~> True

TA-DAH!
--}

{-- BONUS -----------------------------------------------------------------
Read in the file at this directory: Y2017/M02/D07/rosalind_seto.txt, or url:
https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2017/M02/D07/rosalind_seto.txt.gz?raw=true
do a grouper on it and then save the result in proper format as expected by
rosalind.info as ans.txt.
--}

saveSetoAns :: FilePath -> FilePath -> IO ()
saveSetoAns rosalindInput answerFile = 
   BL.readFile rosalindInput                         >>=
   grouper . BL.unpack . decompress                  >>=
   BL.writeFile answerFile . compress . BL.pack . unlines . map show

{--
*Y2017.M02.D07.Solution> saveSetoAns "Y2017/M02/D07/rosalind_seto.txt.gz" "Y2017/M02/D07/ans.txt.gz"

Outputs the correct solution to the rosalind.info seto problem.
--}

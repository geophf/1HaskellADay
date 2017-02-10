{-# LANGUAGE ViewPatterns #-}

module Y2017.M02.D10.Solution where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Codec.Compression.GZip

{--
Today's Haskell exercise is from http://rosalind.info/problems/bins/

Binary Search solved by 1391 as of February 9th, 2017

Binary search is the ultimate divide-and-conquer algorithm. To find a key k
in a large file containing keys A[1..n] in sorted order, we first compare k
with A[n/2], and depending on the result we recurse either on the first half 
of the file, A[1..n/2], or on the second half, A[n/2+1..n]. The recurrence now 
is T(n)=T(n/2)+O(1). Plugging into the master theorem (with a=1,b=2,d=0) we get 
the familiar solution: a running time of just O(logn)

Source: Algorithms by Dasgupta, Papadimitriou, Vazirani. McGraw-Hill. 2006.

Problem

The problem is to find a given set of keys in a given array.

Given: Two positive integers n <= 10e5 and m <= 10e5, a sorted array A[1..n]
of integers from −10e5 to 10e5 and a list of m integers 

−10e5 <= k1,k2,...km <= 10e5

Return: For each ki, output an index 1 <= j <= n such that A[j]=ki or "-1" if 
there is no such index.
--}

sample :: String
sample = unlines ["5","6","10 20 30 40 50","40 10 35 15 40 20"]

result :: String
result = "4 1 -1 -1 4 2"

-- these are all very String-centric, but those are the data-sets you're working
-- with. Deal.

-- Okay, we're dealing with sets of size 20e5. No problem. I've built sets of
-- size 250e5 so data-set size shouldn't be a problem.

-- The first thing we need to do is to parse the input sample

parseInput :: String -> (Int, Int, [Int], [Int])
parseInput (lines -> [l1, l2, l3, l4]) = (read l1, read l2, wmr l3, wmr l4)
   where wmr = map read . words   -- "wmr": words map read

{--
>>> let (_, _, inp, srch) = parseInput sample 
(5,6,[10,20,30,40,50],[40,10,35,15,40,20])
--}

-- I don't know what the n and m values are for, other than a C malloc, so
-- I'm just going to ignore them.

-- So, next we need to convert the input, sorted, array into an indexed array.
-- Now, you can simply put those sorted values into a Data.Array.Array type,
-- but the thing is, really, the indices (or keys) are the data values, and 
-- their location in the array (the 'indices') are really the values we are
-- looking for, n'est-ce pas?

-- So, to my mind, putting the input array into an array is counterproductive.

-- But what is the data structure where we have arbitrary keys that we wish
-- to look up values against?

-- Data.Map.Map

-- Let's put the input array into a Map where the keys are the data values
-- and the values are the 1,2,3,... positions in the source array.

-- How do we do that?

indexArray :: [Int] -> Map Int Int
indexArray = Map.fromList . (`zip` [1..])

{--
>>> let idxt = indexArray inp
fromList [(10,1),(20,2),(30,3),(40,4),(50,5)]
--}

-- Now that we have that, we simply do a lookup to find the array index.

-- But here's the thing: Map.lookup will return Maybe Int, and we need just
-- Int or -1 if there's no index. How do we do that?

maybe2Int :: Maybe Int -> Int
maybe2Int = fromMaybe (-1)

-- You've got the pieces, let's put it all together

binarySearch :: Map Int Int -> [Int] -> [Int]
binarySearch indexed = map (maybe2Int . (`Map.lookup` indexed))

{--
>>> binarySearch idxt srch 
[4,1,-1,-1,4,2]
--}

-- One more step: the output is a list of Int values. Great, but the output
-- format is space-separated values with no list brackets. Convert the answer
-- to the expected format

formattedOutput :: [Int] -> String
formattedOutput = unwords . map show

-- And now, read in a sample file, and write out a result file in the expected
-- format

largeSearch :: FilePath -> FilePath -> IO ()
largeSearch inputFile outputFile =
   BL.readFile inputFile                      >>=
   (\(_, _, inp, srch) -> 
      let idxt = indexArray inp
          ans  = binarySearch idxt srch
      in  BL.writeFile outputFile (compress . BL.pack $ formattedOutput ans))
                     . parseInput . BL.unpack . decompress

{--
*Y2017.M02.D10.Solution> largeSearch "Y2017/M02/D10/rosalind_bins.txt.gz"
                                     "Y2017/M02/D10/ans.txt.gz"

saves out the result to ans.txt.gz at this directory.
--}

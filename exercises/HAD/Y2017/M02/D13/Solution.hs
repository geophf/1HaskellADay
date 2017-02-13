{-# LANGUAGE ViewPatterns #-}

module Y2017.M02.D13.Solution where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Codec.Compression.GZip

-- below import available from 1HaskellADay git repository

import Y2017.M02.D10.Solution (formattedOutput)

{--
Are you having fun solving these rosalind.info puzzles? I am!

Today's comes from http://rosalind.info/problems/maj/

Majority Element solved by 952 as of February 9th, 2017

Problem

An array A[1..n] is said to have a majority element if more than half of its 
entries are the same.

Given: A positive integer k <= 20, a positive integer n <= 10e4, and k arrays of
size n containing positive integers not exceeding 10e5

Return: For each array, output an element of this array occurring strictly more 
than n/2 times if such element exists, and "-1" otherwise.

Source: Algorithms by Dasgupta, Papadimitriou, Vazirani. McGraw-Hill. 2006.
--}

sample :: String
sample = unlines ["4 8","5 5 5 5 5 5 5 5","8 7 7 7 1 7 3 7",
                  "7 1 6 5 10 100 1000 1","5 1 6 7 1 1 10 1"]

result :: String
result = "5 7 -1 -1"

{--
Discussion

It is not difficult to develop a divide-and-conquer algorithm checking whether a
given array of size n contains a majority element in O(nlogn) time. It is 
interesting to note that there is also a linear time algorithm and it is also 
based on divide-and-conquer.
--}

-- "It is not difficult to develop a divide-and-conquer algorithm..." WHAAAA?

-- Okay, anyway, first we need a set of parsers, one for the first line
-- (because obviously, we can't ignore the first line, can we?) and then for
-- the lists that follow

parseMetadata :: String -> (Int, Int)
parseMetadata = (head &&& last) . parseList -- huh.

parseList :: String -> [Int]
parseList = map read . words

-- so we parse the metadata in the first line, then in each line thereafter
-- we parse that line as a list to 'not difficult develop a divide-and-conquer
-- algorithm' to solve the problem. Yeah. Right. lol.

-- Well, anyway.

-- Declaratively, we need to scan a list and determine what is the majority
-- element value is, be it -1 for no such element or x where x is that element

majorityElement :: Int -> [Int] -> Int
majorityElement lengthList (h:t) =
   me (lengthList `div` 2) (pred lengthList) (h,1) (Map.insert h 1 Map.empty) t

me :: Int -> Int -> (Int, Int) -> Map Int Int -> [Int] -> Int
me half _ (ans,cnt) _ [] = if cnt > half then ans else -1
me half rem (ans, cnt) store (h:t) | half > rem + cnt = -1
                                   | cnt  > half      = ans
                                   | otherwise        =
   let newcnt = succ (fromMaybe 0 (Map.lookup h store))
       newans = if newcnt > cnt then (h, newcnt) else (ans, cnt)
   in  me half (pred rem) newans (Map.insert h newcnt store) t

-- now, how you do that, it's up to you.

-- Question. Is it always necessary to scan the entire list each time to find
-- the value of majorityElement? I think it's not necessary. Your thoughts?

{--
>>> majorityElement 8 [5,5,5,5,5,5,5,5]
5
>>> majorityElement 8 [8,7,7,7,1,7,3,7]
7
>>> majorityElement 8 [7,1,6,5,10,100,1000,1]
-1
>>> majorityElement 8 [5,1,6,7,1,1,10,1]
-1

Sweet!
--}

-- okay, so let's tie this together

majorityElements :: String -> [Int]
majorityElements (lines -> (h:t)) =
   map (majorityElement (snd (parseMetadata h)) . parseList) t

{--
>>> majorityElements sample 
[5,7,-1,-1]

Of course, the format of the output is a string. Hint: see Friday's problem for 
a discussion on that (imported above).

>>> formattedOutput (majorityElements sample) == result
True

YES!
--}

-- AND we need to input up to 20 very large lists (with meta data) and output
-- up to 20 majority elements (that we can just visually inspect), so:

largeDataSetMajorityElements :: FilePath -> IO String
largeDataSetMajorityElements =
   fmap (formattedOutput . majorityElements . BL.unpack . decompress)
      . BL.readFile

-- run the above against Y2017/M02/D13/rosalind_maj.txt.gz. What is your result?

{--
>>> largeDataSetMajorityElements "Y2017/M02/D13/rosalind_maj.txt.gz" 
"6149 3124 -1 -1 1822 8124 -1 2071 1917 4973 8845 -1 -1 -1 5067 -1 -1 -1 8539 -1"

This took about 2 seconds to compute against a compressed file of 227kbytes
--}

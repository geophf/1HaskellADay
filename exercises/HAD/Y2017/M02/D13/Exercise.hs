module Y2017.M02.D13.Exercise where

import Codec.Compression.GZip

-- below import available from 1HaskellADay git repository

import Y2017.M02.D10.Exercise (formattedOutput)

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
parseMetadata line = undefined

parseList :: String -> [Int]
parseList line = undefined

-- so we parse the metadata in the first line, then in each line thereafter
-- we parse that line as a list to 'not difficult develop a divide-and-conquer
-- algorithm' to solve the problem. Yeah. Right. lol.

-- Well, anyway.

-- Declaratively, we need to scan a list and determine what is the majority
-- element value is, be it -1 for no such element or x where x is that element

majorityElement :: Int -> [Int] -> Int
majorityElement lengthList list = undefined

-- now, how you do that, it's up to you.

-- Question. Is it always necessary to scan the entire list each time to find
-- the value of majorityElement? I think it's not necessary. Your thoughts?

-- okay, so let's tie this together

majorityElements :: String -> [Int]
majorityElements inputdata = undefined

-- of course, the format of the output is a string. Hint: see Friday's
-- problem for a discussion on that (imported above).

-- AND we need to input up to 20 very large lists (with meta data) and output
-- up to 20 majority elements (that we can just visually inspect), so:

largeDataSetMajorityElements :: FilePath -> IO String
largeDataSetMajorityElements gzippedInputFile = undefined

-- run the above against Y2017/M02/D13/rosalind_maj.txt.gz. What is your result?

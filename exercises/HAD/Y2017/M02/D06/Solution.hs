module Y2017.M02.D06.Solution where

{--
As promised, a Rosalind problem.

This problem is from http://rosalind.info/problems/sset/ and reads thus:

Counting Subsets, solved by 1457 as of February 6th, 2017:

Characters and SNPs

A character is any feature (genetic, physical, etc.) that divides a collection 
of organisms into two separate groups. One commonly used genetic character is 
the possession of a single-nucleotide polymorphism, or SNP.

In a process called genotyping, the SNP markers taken from a large number of 
human donors have been used very successfully to catalogue the migration and 
differentiation of human populations over the last 200,000 years. For $199, you 
can participate in National Geographic's Genographic Project, and discover your 
own genetic heritage.

Whether we use genetic or physical characters, we may think of a collection of n
characters as a collection of "ON"/"OFF" switches. An organism is said to 
possess a character in the "ON" position (although often the assignment of 
"ON"/"OFF" is arbitrary). Given a collection of taxa, we may represent a 
character by the collection of taxa possessing the character.

Problem

A set is the mathematical term for a loose collection of objects, called 
elements. Examples of sets include {the moon, the sun, Wilford Brimley} and ℝ,
the set containing all real numbers. We even have the empty set, represented by 
∅ or {}, which contains no elements at all. Two sets are equal when they contain
the same elements. In other words, in contrast to permutations, the ordering of 
the elements of a set is unimportant (e.g., {the moon, the sun, Wilford Brimley}
is equivalent to {Wilford Brimley, the moon, the sun}). Sets are not allowed to 
contain duplicate elements, so that {Wilford Brimley, the sun, the sun} is not a
set. We have already used sets of 2 elements to represent edges from a graph.

A set A is a subset of B if every element of A is also an element of B, and we 
write A⊆B. For example, {the sun, the moon}⊆{the sun, the moon, Wilford Brimley},
and ∅ is a subset of every set (including itself!).


As illustrated in the biological introduction, we can use subsets to represent 
the collection of taxa possessing a character. However, the number of 
applications is endless; for example, an event in probability can now be defined
as a subset of the set containing all possible outcomes.

Our first question is to count the total number of possible subsets of a given 
set.

Given: A positive integer n, n <= 1000

Return: The total number of subsets of {1,2,…,n} modulo 1,000,000.

Sample Dataset: 3
Sample Output: 8

Hint

What does counting subsets have to do with characters and "ON"/"OFF" switches?
--}

subsetSizeOfSetOfSize :: Integer -> Integer
subsetSizeOfSetOfSize = (`mod` 1000000) . (2 ^)

-- define the above function and solve for n = 972 then n = 994

{--
*Y2017.M02.D06.Solution> subsetSizeOfSetOfSize 972 ~> 557696
*Y2017.M02.D06.Solution> subsetSizeOfSetOfSize 994 ~> 563584
--}

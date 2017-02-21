module Y2017.M02.D21.Exercise where

import Network.HTTP.Conduit

{--
Mortal Fibonacci Rabbits solved by 5142 as of February 20th, 2017

http://rosalind.info/problems/fibd/

Problem

Recall the definition of the Fibonacci numbers from “Rabbits and Recurrence 
Relations”, which followed the recurrence relation F(n)=F(n−1)+F(n−2) and 
assumed that each pair of rabbits reaches maturity in one month and produces a 
single pair of offspring (one male, one female) each subsequent month.

Our aim is to somehow modify this recurrence relation to achieve a dynamic 
programming solution in the case that all rabbits die out after a fixed number 
of months. See Figure 4 for a depiction of a rabbit tree in which rabbits live 
for three months (meaning that they reproduce only twice before dying).

Given: Positive integers n <= 100 and m <= 20

Return: The total number of pairs of rabbits that will remain after the n-th 
month if all rabbits live for m months.
--}

sample :: String
sample = "6 3"

result :: Integer
result = 4

-- this function may help:

fibo :: Integer -> Integer
fibo n = undefined

-- hint: I've seen this function somewhere before...

nRabbitsAtGenerationWithMortality :: Integer -> Integer -> Integer
nRabbitsAtGenerationWithMortality gen mortality = undefined

{-- BONUS -----------------------------------------------------------------

Solve for some larger numbers in the file at this directory at rosalind_fibd.txt
or at the URL:
https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2017/M02/D21/rosalind_fibd.txt
--}

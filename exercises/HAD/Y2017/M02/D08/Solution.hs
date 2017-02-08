module Y2017.M02.D08.Solution where

{--
Today's Haskell problem is from http://rosalind.info/problems/iev/

Calculating Expected Offspring solved by 4657 as of February 7th, 2017

The Need for Averages

Averages arise everywhere. In sports, we want to project the average number of 
games that a team is expected to win; in gambling, we want to project the 
average losses incurred playing blackjack; in business, companies want to 
calculate their average expected sales for the next quarter.

Molecular biology is not immune from the need for averages. Researchers need to 
predict the expected number of antibiotic-resistant pathogenic bacteria in a 
future outbreak, estimate the predicted number of locations in the genome that 
will match a given motif, and study the distribution of alleles throughout an 
evolving population. In this problem, we will begin discussing the third issue; 
first, we need to have a better understanding of what it means to average a 
random process.

Problem

For a random variable X taking integer values between 1 and n, the expected 
value of X is E(X)=∑nk=1k×Pr(X=k). The expected value offers us a way of taking 
the long-term average of a random variable over a large number of trials.

As a motivating example, let X be the number on a six-sided die. Over a large 
number of rolls, we should expect to obtain an average of 3.5 on the die (even 
though it's not possible to roll a 3.5). The formula for expected value 
confirms that E(X)=∑6k=1k×Pr(X=k)=3.5

More generally, a random variable for which every one of a number of equally 
spaced outcomes has the same probability is called a uniform random variable 
(in the die example, this "equal spacing" is equal to 1). We can generalize our 
die example to find that if X is a uniform random variable with minimum 
possible value a and maximum possible value b, then E(X)=a+b2. You may also wish
to verify that for the dice example, if Y is the random variable associated with
the outcome of a second die roll, then E(X+Y)=7

Given: Six positive integers, each of which does not exceed 20,000. The integers
correspond to the number of couples in a population possessing each genotype 
pairing for a given factor. In order, the six given integers represent the 
number of couples having the following genotypes:

1. AA-AA
2. AA-Aa
3. AA-aa
4. Aa-Aa
5. Aa-aa
6. aa-aa

Return: The expected number of offspring displaying the dominant phenotype in 
the next generation, under the assumption that every couple has exactly two 
offspring.

--}

-- Okay, first off, we build a mapping from genotype to expected dominance

xpect :: [Float]
xpect = [2.0,    -- AA-AA both offspring dominant
         2.0,    -- AA-Aa both offspring dominant
         2.0,    -- AA-aa both offspring dominant
         1.5,    -- Aa-Aa 3/4 offspring dominance
         1.0,    -- Aa-aa 50% offspring dominance
         0.0]    -- aa-aa no dominant offspring

sample :: String
sample = "1 0 0 1 0 1"

-- so we need to convert our sample to a list of Ints:

parseInput :: String -> [Int]
parseInput = map read . words

-- then verify our solution is the result below:

result :: Float
result = 3.5

-- So, let's do this: we multiply couples by the expected offspring dominance

expectedOffspring :: [Int] -> Float
expectedOffspring = sum . zipWith (*) xpect . map fromIntegral

-- *Y2017.M02.D08.Solution> expectedOffspring (parseInput sample) ~> 3.5

-- After you verify expectedOffspring works with sample, what is the answer for:

population :: String 
population = "17224 19615 19440 18066 17971 19296"

-- *Y2017.M02.D08.Solution> expectedOffspring (parseInput population) ~> 157628.0

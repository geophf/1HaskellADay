module Y2017.M02.D17.Exercise where

import Data.Map (Map)
import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Data.Probability

import Rosalind.Genotypes

{--
Okay, yesterday we looked at a single characteristic/just one genotype. Of
course, organisms have multiple genotypes, so what characteristics do offspring
inherit from their parents and how when there are multiple genotypes to 
consider?

That is today's Haskell problem.

Let's say several genotypes define the traits of an organism

Something like: ...

kinda Traits = Genotypes [Genotype]
   deriving (Eq, Ord, Show)

Of course, the traits are not simply an arbitrary list of genotypes, because
a genotype is paired only with the genotype of the same characteristic ...

so ... typed genotypes? Ugh. I mean, that certainly is a compile-time guarantee
that the conjugation of genotypes will work, but that seems like an awefully lot
of coding/housekeeping work to make that guarantee.

(the Idris crowd may, or may not, disagree with me)

How about, instead, this:
--}

type Characteristic = Char

data Traits = Genotypes { traits :: Map Characteristic Genotype }
   deriving (Eq, Ord, Show)

-- This way, we can guarantee our parents have the same signature by

type Signature = String

signature :: Traits -> Signature
signature trait = undefined

-- And we also guarantee that a genotype crosses with a genotype of the same
-- characteristic. How? We'll develop that below.

-- 'Below' meaning now.

-- Given two parents each with a set of traits, write a function that proves
-- they both have the same signature

sameSignature :: Traits -> Traits -> Bool
sameSignature parentA parentB = undefined

-- Once we determine that the parents have the same set of genotype (signatures)
-- find the (probability) distribution of the set of traits of their offspring

-- But what's that? Well, we've seen that matching the primary and secondary
-- phenotype in the genotype are independent, so the resulting genotype is the
-- matches of the primaries combined with the matches of the secondaries.
-- 
-- Similarly, we already have in Rosalind.Genotypes matching for genotypes, so
-- we just combine all the paired independent geotype matches. Let's do that

match :: Traits -> Traits -> Prob Traits
match parentA parentB = undefined

-- Let's try it out. What are the possible traits of a child of parents A and B?

parentA, parentB :: Traits
parentA = Genotypes . Map.fromList . zip "ab" . map mkgene $ words "Aa Bb"
parentB = parentA -- parent B has the same traits as parent A.

-- Hint: look to flipThree for combining genotypes into a new set of traits

-- One of the possible children will have the traits "Aa Bb" ... what is the
-- probability of a child having parent A's traits?

traitsOf :: Prob Traits -> Traits -> Float
traitsOf distribution traits = undefined

-- note: the return type is intentionally float even though the probabilities
-- are Rational values. How do we get the returned value to be Float?

{-- BONUS -----------------------------------------------------------------

Meet Thom and his waifu-chan.
--}

thomandwaifu :: (Traits, Traits)
thomandwaifu = (parentA, parentB)

-- thom and waifu-chan have have two boys. What are their traits, probably?

twoboys :: (Traits, Traits) -> (Prob Traits, Prob Traits)
twoboys (fromthom, andwaifu) = undefined

-- The boys marry girls just like mom and have two boys each, respectively.
-- What are the boys' children's traits?

grandsons :: (Prob Traits, Prob Traits) -> Prob Traits
grandsons (boy, girl) = undefined

-- What is the probability of atLeast 1 grandson is "Aa Bb" just like dear old
-- Mom and Dad?

justlikemom :: Prob Traits -> Float
justlikemom distribution = undefined

-- More generally, for generation n, where each member marries an "Aa Bb" wife
-- and has two boys, what is the probability that there are k "Aa Bb" offspring?

moregenerally :: Prob Traits -> Int -> Int -> Float
moregenerally distribution n k = undefined

-- incidentally, the bonus question answers http://rosalind.info/problems/lia/

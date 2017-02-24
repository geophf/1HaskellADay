module Rosalind.Traits where

import Control.Monad (guard, join, liftM2)
import Data.Map (Map)
import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Control.List (takeout)
import Control.Logic.Frege ((<<-), adjoin)
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
signature = Map.keys . traits

-- And we also guarantee that a genotype crosses with a genotype of the same
-- characteristic. How? We'll develop that below.

-- 'Below' meaning now.

-- Given two parents each with a set of traits, write a function that proves
-- they both have the same signature

sameSignature :: Traits -> Traits -> Bool
sameSignature = uncurry (==) <<- curry (adjoin signature)

-- Once we determine that the parents have the same set of genotype (signatures)
-- find the (probability) distribution of the set of traits of their offspring

-- But what's that? Well, we've seen that matching the primary and secondary
-- phenotype in the genotype are independent, so the resulting genotype is the
-- matches of the primaries combined with the matches of the secondaries.
-- 
-- Similarly, we already have in Rosalind.Genotypes matching for genotypes, so
-- we just combine all the paired independent geotype matches. Let's do that

match :: Traits -> Traits -> Prob Traits
match parentA parentB = guard (sameSignature parentA parentB)     >>
   let pa    = Map.toList (traits parentA)
       pb    = traits parentB
       pairs = map (\(sig, trait) -> (sig, cross (pb Map.! sig) trait)) pa

{-- 
pairs = [('a',Prob {getProb = [(AA,1 % 4),(Aa,1 % 2),(aa,1 % 4)]}),
         ('b',Prob {getProb = [(BB,1 % 4),(Bb,1 % 2),(bb,1 % 4)]})]

which we need to convert into Prob [(Genotypes ...,prob), ...]
--}

       genos = map sequence pairs

{--
genos = [Prob {getProb = [(('a',AA),1 % 4),(('a',Aa),1 % 2),(('a',aa),1 % 4)]},
         Prob {getProb = [(('b',BB),1 % 4),(('b',Bb),1 % 2),(('b',bb),1 % 4)]}]

so, from each of these probabilities we need to reformulate them as a set
of a,b, ... probabilities
--}

       mixy  = remix (map getProb genos)

{--
mixy = [[(('a',AA),1 % 4),(('b',BB),1 % 4)],..., 8 more elements]

which is close to where we want to get it. We just have to:
--}

   in  Prob (map ((Genotypes . Map.fromList *** product) . unzip) mixy)

remix :: Eq a => [[(a,Rational)]] -> [[(a,Rational)]]
remix [] = [[]]
remix (p:ps) = takeout p >>= \(x,_) -> remix  ps >>= return . (x:)

-- Let's try it out. What are the possible traits of a child of parents A and B?

parentA, parentB :: Traits
parentA = Genotypes . Map.fromList . zip "ab" . map mkgene $ words "Aa Bb"
parentB = parentA -- parent B has the same traits as parent A.

{--
>>> match parentA parentB
Prob {getProb = [(Genotypes {traits = fromList [('a',AA),('b',BB)]},1 % 16),
                 (Genotypes {traits = fromList [('a',AA),('b',Bb)]},1 % 8),
                 (Genotypes {traits = fromList [('a',AA),('b',bb)]},1 % 16),
                 (Genotypes {traits = fromList [('a',Aa),('b',BB)]},1 % 8),
                 (Genotypes {traits = fromList [('a',Aa),('b',Bb)]},1 % 4),
                 (Genotypes {traits = fromList [('a',Aa),('b',bb)]},1 % 8),
                 (Genotypes {traits = fromList [('a',aa),('b',BB)]},1 % 16),
                 (Genotypes {traits = fromList [('a',aa),('b',Bb)]},1 % 8),
                 (Genotypes {traits = fromList [('a',aa),('b',bb)]},1 % 16)]}

These traits should sum to probability 1:

>>> sum (map snd (getProb $ match parentA parentB))
1 % 1

TA-DAH!
--}

-- One of the possible children will have the traits "Aa Bb" ... what is the
-- probability of a child having parent A's traits?

traitsOf :: Prob Traits -> Traits -> Float
traitsOf dist trait = t' (getProb dist)
   where t' [] = 0
         t' ((h,p):t) | h == trait = fromRational p
                      | otherwise  = t' t

{--
>>> traitsOf ans parentA
0.25

... but:
>>> traitsOf ans (Genotypes (Map.fromList [('a', mkgene "Aa"), ('b', mkgene "Bc")]))
0.0
--}

{-- BONUS -----------------------------------------------------------------

Meet Thom and his waifu-chan.
--}

thomandwaifu :: (Traits, Traits)
thomandwaifu = (parentA, parentB)

-- thom and waifu-chan have have two boys. What are their traits, probably?

twoboys :: (Traits, Traits) -> (Prob Traits, Prob Traits)
twoboys = adjoin (uncurry match) . dup

dup :: a -> (a,a)
dup = join (,)

-- The boys marry girls just like mom and have two boys each, respectively.
-- What are the boys' children's traits?

-- Since the boys are probability distributions, we need a matchP function,
-- just like the crossP function from Rosalind.Genotypes

matchP :: Prob Traits -> Prob Traits -> Prob Traits
matchP = condense . join <<- liftM2 match

{--
>>> let boyz = twoboys thomandwaifu 
>>> let grandchild = uncurry matchP boyz
>>> condense' grandchild Map.! parentA
1 % 4

The probability of a grandchild having the Traits Aa Bb is 25%
--}

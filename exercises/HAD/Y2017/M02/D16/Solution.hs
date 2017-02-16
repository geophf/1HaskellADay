module Y2017.M02.D16.Solution where

import Control.Monad (liftM2, join)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- below import available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin, (<<-))
import Data.Probability

{--
A Genotype is a paired value a trait dominance, so that for some trait, T,
the genotype of a purely dominant trait is TT, mixed (showing dominant) is Tt
and a purely recessive trait is tt.

Note that a mixed trait Tt is always written thus (so Tt == tT, as it were).

Declare a Genotype that takes Char values to represent the alleles.
--}

data Genotype = Gen { alleles :: [Char] }
   deriving (Eq, Ord)

instance Show Genotype where
   show = alleles

mkgene :: String -> Genotype
mkgene = Gen . sort

{--
Now, in conjugation, each parent gene donates either allele with equal 
likelihood to the child gene.

So, for example, you have two parents, one with genotype Aa, the other
with genotype AA, the child has one of the following four possible genes:

 Aa x AA = (AA, AA, Aa, Aa) or, simplifying: (AA, Aa) each option equally likely

So, today's Haskell problem. Given a genotype, one from each parent, define
the function, cross, that gives a probability distribution of the resulting
genotype in the offspring.
--}

-- to do this, we re-present a genotype as a probability distribution

gene2prob :: Genotype -> Prob Char
gene2prob = uniform . alleles

-- which means we need to go back the other way, too:

prob2gene :: Prob Char -> Genotype
prob2gene = mkgene . map fst . getProb

cross :: Genotype -> Genotype -> Prob Genotype
cross a b = condense $ fmap mkgene (mapgene (:) a <*> mapgene pure b)
   where mapgene f = fmap f . gene2prob

-- What is the Genotype of a child whose parents are a = "Aa" b = "AA"?
-- (as a probability distribution)

parentsA, parentsB :: (Genotype, Genotype)
parentsA = adjoin mkgene ("Aa", "AA")

{--
>>> uncurry cross parentsA
Prob {getProb = [(AA,1 % 2),(Aa,1 % 2)]}
--}

-- What is the Genotype of a child whose parents are a = "Aa" b ="Aa"?

parentsB = adjoin mkgene ("Aa", "Aa")

{--
>>> uncurry cross parentsB
Prob {getProb = [(AA,1 % 4),(Aa,1 % 2),(aa,1 % 4)]}
--}

-- Okay, what is the Genotype of the child of the children of parents A and B?
-- That is to say, the grandchild of parentsA and parentsB?
 
-- Hm, how would you define that, as the parents are now the children who
-- are probability distributions. How do we call cross not with plain genotypes,
-- but with probability distribuions?

{--
>>> liftM2 cross childa childb
Prob {getProb = [(Prob {getProb = [(AA,1 % 1)]},1 % 8),
                 (Prob {getProb = [(AA,1 % 2),(Aa,1 % 2)]},1 % 4),
                 (Prob {getProb = [(Aa,1 % 1)]},1 % 8),
                 (Prob {getProb = [(AA,1 % 2),(Aa,1 % 2)]},1 % 8),
                 (Prob {getProb = [(AA,1 % 4),(Aa,1 % 2),(aa,1 % 4)]},1 % 4),
                 (Prob {getProb = [(Aa,1 % 2),(aa,1 % 2)]},1 % 8)]}

ick! But a prob of a prob is joinable so:

>>> join it
Prob {getProb = [(AA,1 % 8),(AA,1 % 8),(Aa,1 % 8),(Aa,1 % 8),(AA,1 % 16),
                 (Aa,1 % 16),(AA,1 % 16),(Aa,1 % 8),(aa,1 % 16),(Aa,1 % 16),
                 (aa,1 % 16)]}

Still ick! but fixable:

>>> condense it
Prob {getProb = [(AA,3 % 8),(Aa,1 % 2),(aa,1 % 8)]}

There we go!

so, let's formalize this:
--}

crossP :: Prob Genotype -> Prob Genotype -> Prob Genotype
crossP = condense . join <<- liftM2 cross

{--
>>> crossP kidA kidB
Prob {getProb = [(AA,3 % 8),(Aa,1 % 2),(aa,1 % 8)]}

>>> let gc = it
--}

-- What is the probability that this grandchild is purely recessive? (aa)
-- What is the probability that this grandchild is purely dominant? (AA)

-- see above: aa 12.5%, AA 37.5%

-- Taking a leaf from the flipThree example from Data.Probability:

-- Let's say the parents have two children each (total of four children)
-- And let's say those children marry (no incest) and let's say those new
-- parents have two children each.

-- we end up with the same distributions (childA, childB and grandchild),
-- just different population sizes.

-- 1. How many grandchildren are there? 4 still. 0 population growth.

-- What is the probability that 1 or more of the children is mixed (Aa)?

-- What is the probability that 2 or more of the children are mixed?

-- Hint: the probability of x outcomes is what? (think: combinations), so
-- the probability of at least x outcomes is x outcomes + x+1 outcomes + ...

-- So we need to find out what the probability of k outcomes is, then:

outcomes :: Ord a => Prob a -> a -> Integer -> Integer -> Rational
outcomes distribution outcome k n =

-- I take: http://math.stackexchange.com/questions/267186/2-heads-or-more-in-3-coin-toss-formula?noredirect=1&lq=1
-- as the directive here

   fromMaybe 0 (fmap ((* choose n k) . toRational . (^ n))
                     (Map.lookup outcome (condense' distribution)))


choose :: Integer -> Integer -> Rational
choose n k = fact n / (fact k * fact (n - k))

fact :: Integer -> Rational
fact = toRational . product . enumFromTo 1

{--
>>> outcomes gc (mkgene "Aa") 1 4
1 % 4
>>> outcomes gc (mkgene "Aa") 2 4
3 % 8
>>> outcomes gc (mkgene "Aa") 3 4
1 % 4
>>> outcomes gc (mkgene "Aa") 4 4
1 % 16
--}

-- Given the above, atLeast becomes a summer function over the ks

atLeast :: Ord a => Prob a -> a -> Integer -> Integer -> Rational
atLeast distribution outcome k n =
   sum (map (flip (outcomes distribution outcome) n) [k .. n])

{--
>>> atLeast gc (mkgene "Aa") 1 4
15 % 16
>>> atLeast gc (mkgene "Aa") 2 4
11 % 16
--}

-- 2. Of course, organisms have more than just one genotype. Tomorrow we'll
-- consider multiple genotypes.

-- This module is being moved to Rosalind.Genotype; outcomes and atLeast being
-- moved to Data.Probability

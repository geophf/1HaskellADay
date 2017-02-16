module Y2017.M02.D16.Exercise where

import Data.Probability

{--
A Genotype is a paired value a trait dominance, so that for some trait, T,
the genotype of a purely dominant trait is TT, mixed (showing dominant) is Tt
and a purely recessive trait is tt.

Note that a mixed trait Tt is always written thus (so Tt == tT, as it were).

Declare a Genotype that takes Char values to represent the alleles.
--}

data Genotype = GenotypeDeclarationYouCreate
   deriving (Eq, Ord, Show)

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

cross :: Genotype -> Genotype -> Prob Genotype
cross a b = undefined

-- What is the Genotype of a child whose parents are a = "Aa" b = "AA"?
-- (as a probability distribution)

parentsA, parentsB :: (Genotype, Genotype)
parentsA = undefined  -- Aa, AA

-- What is the Genotype of a child whose parents are a = "Aa" b ="Aa"?

parentsB = undefined  -- Aa, Aa

-- Okay, what is the Genotype of the child of the children of parents A and B?
-- That is to say, the grandchild of parentsA and parentsB?
 
-- Hm, how would you define that, as the parents are now the children who
-- are probability distributions. How do we call cross not with plain genotypes,
-- but with probability distribuions?

-- What is the probability that this grandchild is purely recessive? (aa)
-- What is the probability that this grandchild is purely dominant? (AA)

-- Taking a leaf from the flipThree example from Data.Probability:

-- Let's say the parents have two children each (total of four children)
-- And let's say those children marry (no incest) and let's say those new
-- parents have two children each.

-- 1. How many grandchildren are there?

-- What is the probability that 1 or more of the children is mixed (Aa)?

-- What is the probability that 2 or more of the children are mixed?

atLeast :: Eq a => Prob a -> a -> Int -> Int -> Rational
atLeast distribution outcome k n = undefined

-- where k is the number of outcomes and n is the total number of events or
-- specifically: k is the number of Aa genotype offspring and n is all children

-- You can also make a an Ord instance if that helps you

-- Hint: the probability of x outcomes is what? (think: combinations), so
-- the probability of at least x outcomes is x outcomes + x+1 outcomes + ...

-- 2. Of course, organisms have more than just one genotype. Tomorrow we'll
-- consider multiple genotypes.

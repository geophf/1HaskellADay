module Y2018.M06.D20.Exercise where

{--
Okay, we're going to do cosine similarity, but to do cosine similarity, we need
vector-sets to do the comparisons. Now, it'd be great to have WordNet, but my

$ cabal install wordnet

fails with 

Ambiguous type variable ‘t0’ arising from an operator section
      prevents the constraint ‘(Foldable t0)’ from being solved.

So, no WordNet for me! Any help, haskellers?

But, in the meantime, we can construct a SymbolTable from all the words in all
the documents and then, for each document, vectorize from that SymbolTable.

The SymbolTable is devoid of meaning, sure, but it does give us document
vectors.
--}

import Data.Array

-- below imports available via 1HaskellADay git repository

import Data.SymbolTable

import Y2018.M06.D19.Exercise

-- from the set of articles from yesterday, construct a symbol table of the
-- words of the text of the articles

{--
>>> arts <- readNonDuplicatedArticles (exDir ++ artJSON)
--}

syms :: [Article] -> SymbolTable
syms arts = undefined

-- Now that we have all our words in a SymbolTable, arrayed from 0 .. n,
-- we can convert the documents into arrays of length n and populate those
-- arrays

type WordVect = Array Int Int

art2vect :: SymbolTable -> Article -> WordVect
art2vect syms article = undefined

-- How many arrays do you have? What is the length of the arrays, n?
-- Also, there may be other approaches than my SymbolTable construction.
-- Thoughts?

{-- BONUS -----------------------------------------------------------------

Compute the cosine similarity for the article set ... eheh, jk: we'll develop
this in future exercises.

--}

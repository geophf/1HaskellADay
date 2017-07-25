module Y2017.M07.D25.Solution where

import Control.Monad

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((-|))
import Relational.Scheme.Types

{--
So we've created and parsed atomic values, we've run (already evaluated)
expressions in our evaluator, the next step Reasoned Schemer takes is defining
the laws of the the relational calculus, and these laws are:

1. Fresh
2. (≡)
3. conde

The funny thing is, that you would think we'd define fresh and be cool with 
that, but no. The Reasoned Schemer demonstrates fresh variables in terms of
(≡) so, to define fresh, we also, or we first, must define (≡).

So, let's define (≡) with atomic, ground terms.
--}

(≡) :: MonadPlus m => Monoid (m ()) => Atom -> Atom -> m ()
p ≡ q = p == q -| return () -- using logic programming to define unification.

{-- that is to say: "If p unifies with q, the statement succeeds."

Given the above definition, what are the results for unifying the below pairs?
--}

unifyAtoms :: [(Atom, Atom)]
unifyAtoms = zip [I 5, B True, L Success, S "hi"] [I 5, S "True", B True, S "Hi"]

{--
>>> zip unifyAtoms ((map (uncurry (≡)) unifyAtoms) :: [[()]])
[((I 5,I 5),[()]),
 ((B True,S "True"),[]),
 ((L #s,B True),[]),
 ((S "hi",S "Hi"),[])]

We see, above, unification attempted on atoms, regardless of their carried type.

Tomorrow, we will unify ground terms (atoms) with free variables and attempt
to unify ground terms to bound variables, and so begin to understand logic
variables.
--}

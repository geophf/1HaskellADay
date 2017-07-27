module Y2017.M07.D26.Solution where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Relational.Scheme.Types
import Relational.Scheme.Unification.Atoms

{--
Unification and freshness for today.

So, yesterday, we saw how to unify two atoms and how that reduced to lifting
their equivalence into the monadic domain.

Basically: unifying two ground terms is monadic guard.

Fine. But what are the rules for unifying variables to ground terms? What
are the rules for unifying variables to other variables, either of which may
be bound or free?

It turns out there are a lot of rules. And we're not even talking about the
occurs-check, yet.

So, like yesterday, where we broke it down: just unification on ground terms,
let's continue to break down the problem into manageable pieces.

There are many behaviors for unification with logic variables, so let's focus
on one aspect of unification at a time.

But even before that, we have to declare what a logic variable is.

What is a logic variable?

data LogicVariable = LV Sym Status
   deriving Eq

The above is saying that a logic variable is named by its symbolic 
representation and its status. What is its symbolic representation, and what
are the statii of a logic variable?

Well, digging deeper, a logic variable is not an independent thing, like an
atom, that can exist without some context. No, a logic variable exists in the
(logic) universe in which it subsides.

So, the above declaration is naïve. Really, a logic variable is a mapping from
its symbolic representation to its status.

Let's talk about the states of a logic variable.

A logic variable exists in three states:

free - it has not been bound to any value
bound - it has been bound to a ground term (e.g.: an atomic value)
linked - it has been linked to another logic variable in any state

(of course, if the other logic variable is free, it, too becomes linked to the
unifying logic variable).

So, let's model the statii of a logic variable:
--}

data Status = Free | Linked { forward, back :: [Symbol] } | Bound Atom
   deriving (Eq, Show)

-- Okay, we know what an atom is (from the import). What's a symbol?

type Symbol = String

-- So we have the statii of logic variables. Now let's create the logic domain
-- for logic variables

type LogicDomain = Map Symbol Status

-- and there you have it. Everything you need have logic variables.

-- so: fresh.

fresh :: MonadPlus m => Symbol -> StateT LogicDomain m ()
fresh sym = get >>= \vars -> case Map.lookup sym vars of
   Nothing   -> put (Map.insert sym Free vars)
   Just Free -> return ()  -- already recorded as a free variable: no-op
   Just x    -> fail ("Logic variable '" ++ sym ++ "' bound: " ++ show x)

{--
>>> runStateT (fresh "a") Map.empty 
((),fromList [("a",Free)])

... what happens when you declare an already existing variable 'fresh'?
... what should happen?
--}

-- which means everything that you do with logic variables include the domain
-- ... including unification. So, let's get to it.

-- What happens when an atom is unified to a variable?

unifyAtom2LV :: MonadPlus m => Atom -> Symbol -> StateT LogicDomain m ()
unifyAtom2LV val var = get >>= \vars -> case Map.lookup var vars of
   Nothing -> bind var val vars
   Just Free -> bind var val vars
   Just (Bound val2) -> unifyAtoms val val2

bind :: MonadPlus m => Symbol -> Atom -> LogicDomain -> StateT LogicDomain m ()
bind var val = put . Map.insert var (Bound val)

{--
So, what needs to happen is:

If the logic variable is free, it needs to be bound to that value.
If the logic variable is bound to a value, it needs to be equal to that value.

... we won't look at linked logic variables today.

Question: What happens when a variable isn't in the domain? What should happen?

What happens is we create a fresh logic variable and add it to the domain with
its newly bound value.
--}

-- What happens when a logic variable is unified to an atom?

unifyLV2Atom :: MonadPlus m => Symbol -> Atom -> StateT LogicDomain m ()
unifyLV2Atom = flip unifyAtom2LV 

-- With the above definitions, unify the following:

logicVars :: [Symbol]
logicVars = words "a b c d"

vals :: [Atom]
vals = map read (words "5 #t #s Hi")

{--
>>> vals
[I 5,B True,L #s,S "Hi"]

>>> runStateT (mapM_ (uncurry unifyLV2Atom) (zip logicVars vals)) Map.empty
((),fromList [("a",Bound (I 5)),("b",Bound (B True)),
              ("c",Bound (L #s)),("d",Bound (S "Hi"))])

>>> lvstate = snd it
--}

-- Now, with those unified variables (that is to say: keep the state active),
-- unify a, b, c, d against the below newvals. What are your results?

newvals :: [Atom]
newvals = map read (words "5 True #u hi")

{--
>>> newvals 
[I 5,S "True",L #u,S "hi"]

>>> runStateT (mapM (uncurry unifyLV2Atom) (zip logicVars newvals)) lvstate
*** Exception: user error (mzero)

Well, it fails, ... and rather spectacularly, too!

We'll look at failures and graceful recovery when we start to execute clauses.

But what happens when we run it against the original value-set?

>>> runStateT (mapM (uncurry unifyLV2Atom) (zip logicVars vals)) lvstate
([(),(),(),()],fromList [("a",Bound (I 5)),("b",Bound (B True)),
                         ("c",Bound (L #s)),("d",Bound (S "Hi"))])

Success! We've unified values against bound variables!
--}

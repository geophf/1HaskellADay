module Y2017.M07.D26.Exercise where

import Control.Monad.State
import Data.Map (Map)

-- below import available via 1HaskellADay git repository

import Relational.Scheme.Types

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

-- (please ignore the Linked status for the time being)
-- Okay, we know what an atom is (from the import). What's a symbol?

type Symbol = String

-- So we have the statii of logic variables. Now let's create the logic domain
-- for logic variables

type LogicDomain = Map Symbol Status

-- and there you have it. Everything you need have logic variables.

-- so: fresh.

fresh :: MonadPlus m => Symbol -> StateT LogicDomain m ()
fresh sym = undefined

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
unifyAtom2LV val var = undefined

{--
So, what needs to happen is:

If the logic variable is free, it needs to be bound to that value.
If the logic variable is bound to a value, it needs to be equal to that value.

... we won't look at linked logic variables today.

Question: What happens when a variable isn't in the domain? What should happen?
--}

-- What happens when a logic variable is unified to an atom?

unifyLV2Atom :: MonadPlus m => Symbol -> Atom -> StateT LogicDomain m ()
unifyLV2Atom var val = undefined

-- With the above definitions, unify the following:

logicVars :: [Symbol]
logicVars = words "a b c d"

vals :: [Atom]
vals = map read (words "5 #t #s Hi")

{--
>>> vals
[I 5,B True,L #s,S "Hi"]
--}

-- Now, with those unified variables (that is to say: keep the state active),
-- unify a, b, c, d against the below newvals. What are your results?

newvals :: [Atom]
newvals = map read (words "5 True #u hi")

{--
>>> newvals 
[I 5,S "True",L #u,S "hi"]
--}

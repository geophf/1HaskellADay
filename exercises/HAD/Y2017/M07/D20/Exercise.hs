module Y2017.M07.D20.Exercise where

import Prelude hiding (fail)
import Control.Monad hiding (fail)

-- below import available via 1HaskellADay git repository

import Relational.Scheme.Types

{--
So, yesterday, we did the types and parsers/readers for atomic values in our
Scheme DSL/Domain Specific Language, so the next logical step is to do lists
in Scheme?

Not according to the Reasoned Schemer. Their next step, before going into the
laws of fresh, (≡), and conde, is to play around with run* for awhile.

What is run*?
--}

data Fresh
data Expr = E ([Fresh] -> LogicValue)
data List
data Value = A Atom | L List

runx :: [Fresh] -> [Expr] -> [[Value]]
runx vars exprs = undefined

{--
Whoa. Wut?

Okay, I showed you the above, but we are NOT going to tackle that as today's
Haskell problem ...

... you're welcome ...

because we must first lay some groundwork to work our way up to run*, like:

* what is Fresh?
* how is an expression evaluated?
* what is a list? (it's own problem for a day)
* what is a value?

All that stuff.*

* stuff, n.: a technical term with a very specific meaning. "Stuff" means stuff.

But even before that, we need to know what #s and #u, the values of LogicValue,
signal to the run*-evaluator.

Fortunately, in Haskell, we have monads, so the meanings of #s and #u are
reductive:

#s: the evaluation of this expression is a success; the evaluation may proceed.
#u: the evaluation of this expression failed; the evaluation terminates.

Sounds familiar?

Define the evaluation of #s and #u monadically. Or, put another way, define
success and fail.
--}

runLogic :: MonadPlus m => LogicValue -> m ()
runLogic lv = undefined

success, fail :: MonadPlus m => m ()
success = runLogic Success
fail    = runLogic Fail

{--
>>> fail :: [()]
[]
>>> success :: [()]
[()]
--}

{--
Great! We still have a long way to go before we define run*, but let's take
the next step. As Expr reduces to #s or #u, let's define runEvalExpr which 
succeeds if all the evaluated expressions succeed and fails if any evaluated 
expression fails. Now, again, we're not going to evaluate expressions until
we iron out some details, so let's simply provide the evaluated expressions
reduced to their LogicValue terms "by hand," as it were.
--}

runEvalExpr :: MonadPlus m => [LogicValue] -> m ()
runEvalExpr vals = undefined

-- once defined, run against the following evaluated expressions

a, b, c :: [LogicValue]
a = replicate 5 Success
b = Fail : a
c = a ++ b

{--
>>> a
[#s,#s,#s,#s,#s]
>>> b
[#u,#s,#s,#s,#s,#s]
>>> c
[#s,#s,#s,#s,#s,#u,#s,#s,#s,#s,#s]

>>> runEvalExpr a :: [()]
[()]
>>> runEvalExpr b :: [()]
[]
>>> runEvalExpr c :: [()]
[]
>>> runEvalExpr a :: Maybe ()
Just ()
>>> runEvalExpr b :: Maybe ()
Nothing
>>> runEvalExpr c :: Maybe ()
Nothing
--}

module Y2018.M03.D05.Exercise where

{--
I'm going to do something a bit quixotic.

"Introduction to Logic
and to the Methodology
of Deductive Sciences,"
by Alfred Tarski, 1941

I know I'm tilting at windmills here, because Gödel blew all this foundational
work out of the water with his paper on the undecidability of these systems.

But still! Listen to Tarski's aim from his preface:

"[Logic] seeks to create a unified conceptual apparatus which would supply a
common basis for the whole of human knowledge."

Talk about vision!

Okay, so, let's build this conceptual apparatus for the whole of human knowledge

Chapter I: On the Use of Variables

1. Constants and Variables

"Every scientific theory is a system of sentences which are accepted as true
and which may be called LAWS OF ASSERTED STATEMENTS or, for short, simply
STATEMENTS. [PROOFS and THEOREMS are STATEMENTS following one another in a
definite order discussed at some other time, not today]

Among the terms and symbols occuring in mathematical theorems and proofs, we
distinguish CONSTANTS and VARIABLES.

In arithmetic, for instance, we encounter such constants as "number",
"zero" ("0"), "one" ("1"), "sum" ("+") and many others. Each of the terms has a
well-determined meaning which remains unchanged throughout the course of the
consideration.

As variables we employ, as a rule, single letters, e.g. in arithmetic the small
letters of the English alphabet: "a", "b", "c", ..., "x", "y", "z". As opposed
to the constants, the variables do not possess any meaning by themselves. Thus,
the question:

	does zero have such and such a property?

e.g.:

	is zero an integer?

can be answered in the affirmative or in the negative; the answer may be true
or false, but at any rate it is meaningful. A question concerning z, on the 
other hand, for example, the question:

	is x an integer?

cannot be answered meaningfully.

... [elide rant against the typing of variables.] ..."

Okay, that section was preliminary, now let's get to today's Haskell problem-set

2. Expressions containing variables – sentiential and designatory functions.

"... An expression of this kind [above], which contains variables and, on
replacement of these variables by constants, become a sentence, is called a
SENTENTIAL FUNCTION. ...; and sentential functions and sentences which are
composed entirely of mathematical symbols, ... such as:

	x + y = 5

are usually referred to in mathematics as FORMULAS. In place of "sentential
function" we shall sometimes simply say "sentence" ...

Besides the sentential functions there are some further expressions containing
variables that merit our attention, namely, the so-called DESIGNATORY or
DESCRIPTIVE FUNCTIONS. They are expressions that, on replacement of the
variables by constants, turn into designations ("descriptions") of things. For
example, the expression:

	2x + 1

is a designatory function, because we obtain the designation of a certain number
(e.g. the number 5), if we replace the variable "x" by an arbitrary numerical
constant, that is, by a constant denoting a number (e.g. "2")

... [elide conversation about equations (which are sentential functions),
and variable substitution by numbers (not operations nor non-numbers) to be
in the domain of arithmetic] ..."

Okay, so, given sections 1 and 2 above, we have enough to go onto the
first set of exercises in chapter 1 of "Introduction to Logic."

Let's break it down. We know that SENTENCES and (DESCRIPTIVE) FUNCTIONS are
composed of CONSTANTS (which can be operators, as you recall) and VARIABLES,
and these can occur at any place in a sentence or function.
--}

data ATOM k = CONSTANT k | VARIABLE k | PAREN (FUNCTION k)

-- dat last one, doe. But, yes, a parenthesize expression is atomic, so ...

-- and have some pretty representation of an atom?

instance Show k => Show (ATOM k) where
   show atom = undefined

{--
Now, here's the tough part. We can't presuppose, or, more correctly, superimpose
our assumptions on k. Because k is "+" or "5" or "=" or "x" or "y" or whatever.
But making k String is ... icky.

What is k?
--}

data K = YouDeclareTheseValues  -- use the questions below to guide you here
   deriving Eq

instance Show K where
   show k = undefined

{-- 
We also distinguish between SENTENCE and FUNCTION in that a sentence has proof 
or validity when all its variables are substituted and a function has a value.

We can also state, implied by the text, that a sentence as a sole equational
differentiator (=, <, >, either solely or (optionally) paired to form a 
compound equational resolutor), whereas a FUNCTION does not.

Again, implied, or, put another way: a SENTENCE is two FUNCTION expressions
tied together into an equational form.

... nope because x - 3 = 0 is a sentence, so we have to allow either expressions
or atoms on either side... hm.
--}

data FUNCTION k = Fun [ATOM k]

instance Show k => Show (FUNCTION k) where
   show fn = undefined

data RESOLUTOR = Singlet Ordering | Doublet Ordering Ordering
   deriving (Eq, Ord)

instance Show RESOLUTOR where
   show r = undefined

data EXPRESSION k = Expr (FUNCTION k) | Atom (ATOM k)
   deriving Show

data SENTENCE k = Eqation (EXPRESSION k) RESOLUTOR (EXPRESSION k)

instance Show k => Show (SENTENCE k) where
   show eq = undefined

{--
Do you have all these types declared? Great. Now answer the following question:
the first part formulated by Alfred, the second part by moi-self.

1a. Which among the following expressions are sentential functions, and which
    are designatory functions?

(a) x is divisible by 3
(b) the sum of the numbers x and 2
(c) y^2 - z^2
(d) y^2 = z^2
(e) x + 2 < y + 3
(f) (x + 3) - (y + 5)

-- BONUS ------------------
(g) the mother of x and z -- actually, this one isn't trivial, ... why?
(h) is x the mother of z? -- and this one is trivial

1b. Formulate each of the above in the Tarski language. Show their value
printed out in the Haskell REPL.

(I made up the name for this language, just now)
--}

-- (you may want to set up some helper functions to write the above 
-- expressions)

a, b, c, d, e, f :: Either (SENTENCE K) (FUNCTION K)
a = undefined
b = undefined
c = undefined
d = undefined
e = undefined
f = undefined

{-- BONUS -----------------------------------------------------------------

2. Give examples of sentential and designator functions from the field of
geometry.

What? Don't give me that look. I'm just the messenger here. Tarski thought
problem 2 was simple enough to be included in chapter I, so have at it!
--}

-- Tomorrow we'll look at broad strokes of solving arithmetic sentential 
-- functions. Then we'll look at Existential vs Universal variables. After that
-- we'll look at bound vs free variables.

{-# LANGUAGE ExistentialQuantification #-}

module Y2018.M03.D05.Solution where

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

import Data.List (intercalate)

-- below import available via 1HaskellADay git repository

import Control.Presentation (laxmi)

data ATOM k = CONSTANT k | VARIABLE k | PAREN (FUNCTION k)

-- dat last one, doe. But, yes, a parenthesize expression is atomic, so ...

-- and have some pretty representation of an atom?

instance Show k => Show (ATOM k) where
   show (CONSTANT k) = show k
   show (VARIABLE v) = show v
   show (PAREN p)    = '(':show p ++ ")"

-- of course, this makes Read instance harder to decide...

{--
Now, here's the tough part. We can't presuppose, or, more correctly, superimpose
our assumptions on k. Because k is "+" or "5" or "=" or "x" or "y" or whatever.
But making k String is ... icky.

What is k?
--}

data K = Int Integer | Str String | forall a. Op String (a -> a -> a)
       | Num Rational | Var Char
       -- ... etc

-- ... which, of course, is weird, as CONSTANT (Var 'x') is what, precisely?
-- I'm thinking we have to bifurcate constants and variables here, but then
-- that makes expressing functions all the harder because then we get into
-- creating a language with an AST/Abstract Syntax Tree. Hm.

instance Show K where
   show (Int x) = show x
   show (Str s) = show s
   show (Op name f) = name
   show (Num r) = laxmi 4 r
   show (Var x) = [x]

{-- 
We also distinguish between SENTENCE and FUNCTION in that a sentence has proof 
or validity when all its variables are substituted and a function has a value.

We can also state, implied by the text, that a sentence as a sole equational
differentiator (=, <, >, either solely or (optionally) paired to form a 
compound equational resolutor), whereas a FUNCTION does not.

Again, implied, or, put another way: a SENTENCE is two FUNCTION expressions
tied together into an equational form.
--}

data FUNCTION k = Fun [ATOM k]

spacy :: [String] -> String
spacy = intercalate " "

instance Show k => Show (FUNCTION k) where
   show (Fun fs) = spacy (map show fs)

data RESOLUTOR = Singlet Ordering | Doublet Ordering Ordering
   deriving (Eq, Ord)

instance Show RESOLUTOR where
   show (Singlet o) = showOrder o
   show (Doublet o1 o2) = concatMap showOrder [o1, o2]

showOrder :: Ordering -> String
showOrder LT = "<"
showOrder GT = ">"
showOrder EQ = "="

data EXPRESSION k = Expr (FUNCTION k) | Atom (ATOM k)
   deriving Show

data SENTENCE k = Equation (EXPRESSION k) RESOLUTOR (EXPRESSION k)

instance Show k => Show (SENTENCE k) where
   show (Equation f1 o f2) = spacy [show f1, show o, show f2]

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

-- first, some functions to help us writing these expressions...

add, pow, (%), minus :: K
(%)   = Op "%" mod
add   = Op "+" (+)
pow   = Op "^" (^)
minus = Op "-" (-)

sq :: Char -> [ATOM K] -- um ... what? no composable functions?
sq var = [VARIABLE (Var var), CONSTANT pow, CONSTANT (Int 2)]

-- okay, two birds with one stone:

a, b, c, d, e, f :: Either (SENTENCE K) (FUNCTION K)
a = Left (Equation (Expr
                    (Fun [VARIABLE (Var 'x'), CONSTANT (%), CONSTANT (Int 3)])) 
                   (Singlet EQ)
                   (Atom (CONSTANT (Int 0))))

{--
>>> a
Left Expr x % 3 = Atom 0
--}

b = Right (Fun [VARIABLE (Var 'x'), CONSTANT add, CONSTANT (Int 2)])

{--
>>> b
Right x + 2
--}

c = Right (Fun (sq 'y' ++ (CONSTANT minus:sq 'z')))

{--
>>> c
Right y ^ 2 - z ^ 2
--}

d = Left (Equation (Expr (Fun (sq 'y'))) (Singlet EQ) (Expr (Fun (sq 'z'))))

{--
>>> d
Left Expr y ^ 2 = Expr z ^ 2
--}

e = Left (Equation (Expr (Fun [VARIABLE (Var 'x'), CONSTANT add, CONSTANT (Int 2)]))
                   (Singlet LT)
                   (Expr (Fun [VARIABLE (Var 'y'), CONSTANT add, CONSTANT (Int 3)])))

{--
>>> e
Left Expr x + 2 < Expr y + 3
--}

f = Right (Fun [PAREN (Fun [VARIABLE (Var 'x'), CONSTANT add, CONSTANT (Int 3)]),
                CONSTANT minus,
                PAREN (Fun [VARIABLE (Var 'y'), CONSTANT add, CONSTANT (Int 5)])])

{--
>>> f
Right (x + 3) - (y + 5)
--}

{--
Self-critique:

Fun [ATOM k] is just plain ick. It says, in effect: "I trust you, user, to
construct expressions with correct syntax." And, me, being me, did just that,
but it would be ... 'nice' for this 'compiler' to verify expressions are
valid before they reach the evaluator, wouldn't it?

... Sounding more and more like AST/Abstract Syntax Tree is essential for
language-development. Fancy that, Hedda!

Of course, my aim is not to build a language, but to express concepts in a
logical framework.

I believe I just contradicted myself, didn't I. That is to say: is 'language'
expressing concepts in a logical framework? I feel myself getting all
existential here, but that's for tomorrow's exercise. Not today. Good night!
--}

{-- BONUS -----------------------------------------------------------------

2. Give examples of sentential and designator functions from the field of
geometry.

What? Don't give me that look. I'm just the messenger here. Tarski thought
problem 2 was simple enough to be included in chapter I, so have at it!
--}

-- Tomorrow we'll look at broad strokes of solving arithmetic sentential 
-- functions. Then we'll look at Existential vs Universal variables. After that
-- we'll look at bound vs free variables.

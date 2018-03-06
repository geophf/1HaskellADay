module Y2018.M03.D06.Exercise where

{--
Okay, I'm supposed to go into Chapter I.3 of "Introduction to Logic" by Tarski
and talk about existential vs universal quantification of variables today.

And I will talk about that, but not, as it turns out, today. Today I'm going
to go off Tarski's script of classical logic and dive into curried functions.

Tarski has a classical understanding of functions-as-(binary)-operators, and
that's fine for the examples he supplied, but we are informed by Curry and
Howard and Hindley and Minler that there is a different way (or, more correctly,
there are more than a few different ways) to look at functions. Why do I say
this? Because Tarski admits it himself, even unknowingly. For example these
functions:

plus, minus, mod, pow

all fall into the 'operator' category of functions: a -> a -> a

But exercises g and h from yesterday:

motherOf :: a -> a

does not.

We can express the operators in Tarski's Chapter I sketch, but how do we
express motherOf? Today,

Let's talk about functions, baby.

Given the curried function (a -> b) we can express anything, presumably.

Let's try.

But, what are a and b's types? Anything, right? So a is some kind of value
and be is some kind of value, and the function (a -> b), itself, is some kind
of value.

Can we capture this?
--}

-- below import available via 1HaskellADay git repository

import Y2018.M03.D05.Exercise (SENTENCE)

data Fun a b = Fn (a -> b)

-- so that --^ is one approach, the other is --v

data Arr a b = Arr a b
   deriving (Eq, Show)

-- which one works better for you? Why?

-- okay, hot shot: express plus :: Int -> Int -> Int as a Fun-type.

plus :: Fun a b
plus = undefined  -- what does the function plus return in the Fun-domain?

-- or, if you prefer:

plusA :: Arr a b
plusA = undefined -- what does the function plusA return in the Arrow-domain?

-- What is 3 + 4? Or, more generally, what are the sums of these pairs of 
-- numbers using your plus or plusA function?

nums :: [(Int, Int)]
nums = [(75,30),(85,8),(6,5),(71,100),(74,28),(22,84),
        (82,78),(61,81),(21,46),(44,26),(39,11),(80,52),
        (25,57),(79,18),(48,80),(32,61),(21,43),(5,30),
        (98,67),(71,60),(68,84),(86,18),(98,97),(29,4)]

-- nums generated at random.org

sum :: Int -> Int -> Int
sum x y = undefined

-- Now, how about the motherOf function? What does that look like as a Fun?

motherOf :: Fun a b
motherOf = undefined

-- or as an Arr?

motherOfA :: Arr a b
motherOfA = undefined

-- If Pam is the mother of Jenny and Tom, write a Sentential Function that
-- expresses this truth using either the Fun or Arr type to represent functions
-- (see yesterday's exercise for the structure of Sentential Functions)

data Person = Pam | Jenny | Tom
   deriving (Eq, Show)

mom :: Person -> Person -> Person -> SENTENCE Person
mom m child1 child2 = undefined

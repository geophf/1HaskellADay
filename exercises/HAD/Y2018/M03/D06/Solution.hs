module Y2018.M03.D06.Solution where

{--
Okay, I'm supposed to go into Chapter I.3 of "Introduction to Logic" by Tarski
and talk about existential vs universal quantification of variables today.

And I will talk about that, but not, as it turns out, today. Today I'm going
to go off Tarski's script of classical logic and dive into curried functions.

Tarski has a classical understanding of functions-as-(binary)-operators, and
that's fine for the examples he supplied, but we are informed by Curry and
Howard and Hindley and Minler that there is a different way (or, more correctly,
there are more than a few different ways) to look at functions. Today,

Let's talk about functions, baby.

Given the curried function (a -> b) we can express anything, presumably.

Let's try.

But, what are a and b's types? Anything, right? So a is some kind of value
and be is some kind of value, and the function (a -> b), itself, is some kind
of value.

Can we capture this?
--}

import Prelude hiding (sum)

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map

-- below import available via 1HaskellADay git repository

import Y2018.M03.D05.Exercise (SENTENCE)

data Fun a b = Fn { no :: a -> b }

-- geddit? no fun? GEDDIT?

-- so that --^ is one approach, the other is --v

data Arr a b = Arr { straight :: a, true :: b }
   deriving (Eq, Show)

-- because the Black Arr flights straight and true into the heart of Smaug.

-- which one works better for you? Why?

-- okay, hot shot: express plus :: Int -> Int -> Int as a Fun-type.

{--
plus :: Int -> Int -> Fun Int (Fun Int Int)
plus x y = Fn (V x) (V (Fn (V y) (V (x + y))))

But this isn't correct... what if you pass plus a value to be evaluated?
--}

plus :: Num a => a -> Fun a a
plus = Fn . (+)

-- or, if you prefer:

plusA :: Num a => a -> Arr a (a -> Arr a a)
plusA x = Arr x (\y -> Arr y (x + y))

-- What is 3 + 4? Or, more generally, what are the sums of these pairs of
-- numbers using your plus or plusA function?

nums :: [(Int, Int)]
nums = [(75,30),(85,8),(6,5),(71,100),(74,28),(22,84),
        (82,78),(61,81),(21,46),(44,26),(39,11),(80,52),
        (25,57),(79,18),(48,80),(32,61),(21,43),(5,30),
        (98,67),(71,60),(68,84),(86,18),(98,97),(29,4)]

-- nums generated at random.org

sum, sumA :: Int -> Int -> Int
sum = no . plus
sumA x = true . true (plusA x)

{--
>>> map (id &&& uncurry sum) nums
[((75,30),105),((85,8),93),((6,5),11),((71,100),171),((74,28),102),
 ((22,84),106),((82,78),160),((61,81),142),((21,46),67),((44,26),70),
 ((39,11),50),((80,52),132),((25,57),82),((79,18),97),((48,80),128),
 ((32,61),93),((21,43),64),((5,30),35),((98,67),165),((71,60),131),
 ((68,84),152),((86,18),104),((98,97),195),((29,4),33)]

>>> map (uncurry sum) nums == map (uncurry sumA) nums
True
--}

-- Now, how about the motherOf function? What does that look like as a Fun?

-- well, it's comonadic, isn't it? Or a Reader function, right? Because we
-- need the ontology of mothers, the context, in order to answer the question:
-- motherOf Tom or whatever.

motherContext :: Map Person Person
motherContext = Map.fromList [(Jenny, Pam), (Tom, Pam)]

-- now you're going to ask: "Who's the mother of Pam?" But you can't fool me!
-- IT'S MOTHERS! ALL THE WAY DOWN!

motherOf :: Fun Person (Maybe Person)
motherOf = Fn (flip Map.lookup motherContext)

{--
>>> no motherOf Tom
Just Pam

which is a weird way to say it, but okay
--}

-- or as an Arr?

motherOfA :: Person -> Arr Person (Maybe Person)
motherOfA pers = Arr pers (Map.lookup pers motherContext)

{--
>>> motherOfA Jenny 
Arr {straight = Jenny, true = Just Pam}
--}

-- If Pam is the mother of Jenny and Tom, write a Sentential Function that
-- expresses this truth using either the Fun or Arr type to represent functions
-- (see yesterday's exercise for the structure of Sentential Functions)

data Person = Pam | Jenny | Tom
   deriving (Eq, Ord, Show)

-- now here comes the fun part, because SENTENCE has EXPRESSION and EXPRESSION
-- has an underlying FUNCTION that is incompatible with what we have.

-- How do we fix this?

-- Well, I guess we redeclare SENTENCE

-- but that is an exercise for another day, I'm thinking.
mom :: Person -> Person -> Person -> SENTENCE Person
mom m child1 child2 = undefined


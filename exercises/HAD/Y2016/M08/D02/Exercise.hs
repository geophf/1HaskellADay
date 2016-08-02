module Y2016.M08.D02.Exercise where

import Data.Monoid

{--
So, yesterday, we were able to show the digits that added up to a sum, 
constrained by how many digits were permitted.

Great.

Today, we have a similar but different problem.

Let's say we know none, one, or some of the numbers already.

For example for the sum 10, we know the second number of a three number sum is
7, so our answer then is:

[[1,7,2],[2,7,1]]

Solve this problem. That is: you are given a schema that includes blanks and
known numbers and the sought sum, the value returned is the schema completed
with all the slots filled with known numbers... or the empty list if the schema
cannot sum to the sought number.
--}

type Digit = Sum Int
data VNum = Known Digit | Blank deriving (Eq, Ord, Show)

type Schema = [VNum]
type Schemata = [Schema]

scheme :: Sum Int -> Schema -> Schemata
scheme total template = undefined

-- Solve the following:

schProblems :: [(Sum Int, Schema)]
schProblems = [(Sum 4, [Blank, Known (Sum 1)]),                   -- 1
               (Sum 23, [Blank, Blank, Known (Sum 1), Blank]),    -- 2
               (Sum 11, [Blank, Blank, Known (Sum 1), Blank]),    -- 3
               (Sum 23, [Blank, Blank, Blank]),                   -- 4
               (Sum 20, [Blank, Blank, Blank, Blank, Blank])]     -- 5

{--
The solution for 1 should be [[Known (Sum 3), Known (Sum 1)]]

I'm being clever. 2 and 3 actually cross at the third cell, and I 'just so 
happen' to know that value is Known (Sum 1). We'll look at crossing points
in detail another day.

Also, and for consideration for another day (not today) 5: we know a 6 of three 
cells crosses at its second point so we 'know' that the second cell of 5 is one
of the following values: map (Known . Sum) [1,2,3] ... how do we constrain that
second cell? We'll look at that another day.
--}

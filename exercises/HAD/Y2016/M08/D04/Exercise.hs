module Y2016.M08.D04.Exercise where

{--
So, today we ask, "How do we solve a problem like Maria?"

No, wait. That's the wrong movie.

Okay, so, before we solved rows of sums that were either unconstrained or were
completely constrained, that is: we solved the summer-problem (unconstrained
values to sum to a total), or we solved the schema-problem that gave free slots
and slots that had a known value already.

Fine. Good.

Now we look at constraining a cell to a set of values. How do we do that?

Let's look at a particular example.

Let's say we have a sum 20 of three slots, ... easy enough.
But crossing that sum in the first column is a sum of 16 of two slots, ... okay
AND in the second column is a sum of 4 of two slots.

So, our simple sum 20 of three slots is a bit more constrained now

slot 1 can be 7 or 9 (because ...?)
slot 2 can be 1 or 3 (because ...?)
and slot 3 is unconstrained

given that no number is the same as any other in the sum.

How do we roll these constraints into our solver?

Let's do that today.
--}

data Cell = Unconstrained | ConstrainedTo [Int] | Known Int
   deriving (Eq, Ord, Show)

type Schema = [Cell]
type Schemata = [Schema]

solver :: Schema -> Int -> Schemata
solver = undefined

-- Given the above definition of solver, what are the solutions for:

constrainedRows :: [(Schema, Int)]
constrainedRows =
   [([ConstrainedTo [7,9], ConstrainedTo [1,3], Unconstrained], 20),  -- 1
    ([Unconstrained, ConstrainedTo [8,9], ConstrainedTo [1,3]], 13),  -- 2
    ([Unconstrained, ConstrainedTo [8,9], ConstrainedTo [1,3]], 20),  -- 3
    (replicate 4 Unconstrained, 25)]                                  -- 4

-- The solution for -- 1 should return a value:
-- [[Known 9, Known 3, Known 8]]

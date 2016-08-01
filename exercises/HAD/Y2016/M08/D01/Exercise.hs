module Y2016.M08.D01.Exercise where

{--
The Numbers Game!

So, you have a number of variables of integer values that sum to a number. These
numbers must all be different.

Do it, to it.

Or, more specifically: Given a number of 'slots' and a value these slots, when
filled, sum to, give the value of these slots:
--}

summer :: Int -> Int -> [[Int]]
summer sum slotCount = undefined

-- Note the return value: there may be multiple solutions.
-- Note also [1,2,3] for the sum of 6 is a different value than [3,2,1]

-- Question: What are the solutions to the pairs (sum,slots):
-- (4,2), (10,4), (17,2), (12,2), (13,4)

-- Answer (for (4,2)) is: [[1,3],[3,1]]

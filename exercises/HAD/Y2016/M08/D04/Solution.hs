module Y2016.M08.D04.Solution where

import Control.Logic.Frege (assert)
import Data.QBit

-- Let's support the structure we established.

data Cell = Unconstrained | ConstrainedTo [Int] | Known Int
   deriving (Eq, Ord, Show)

type Schema = [Cell]
type Schemata = [Schema]

-- the thing is: qbits are also very good working with constrained unknowns

cell2qbit :: Cell -> QBit Int
cell2qbit Unconstrained      = free
cell2qbit (Known k)          = Observed k
cell2qbit (ConstrainedTo cs) = constrain (`elem` cs)

-- so we simply shift to the QBit category, solve, then shift back to Cells

solver :: Schema -> Int -> Schemata
solver scheme tot = draws (map cell2qbit scheme) [1..9] >>=
   map (fmap Known) . assert ((== tot) . sum) . map extract . fst

-- Given the above definition of solver, what are the solutions for:

constrainedRows :: [(Schema, Int)]
constrainedRows =
   [([ConstrainedTo [7,9], ConstrainedTo [1,3], Unconstrained], 20),  -- 1
    ([Unconstrained, ConstrainedTo [8,9], ConstrainedTo [1,3]], 13),  -- 2
    ([Unconstrained, ConstrainedTo [8,9], ConstrainedTo [1,3]], 20),  -- 3
    (replicate 4 Unconstrained, 25)]                                  -- 4

-- The solution for -- 1 should return a value:
-- [[Known 9, Known 3, Known 8]]

{--
*Y2016.M08.D04.Solution> uncurry solver (head constrainedRows) ~>
[[Known 9,Known 3,Known 8]]
*Y2016.M08.D04.Solution> mapM_ (print . uncurry solver) constrainedRows 
[[Known 9,Known 3,Known 8]]
[[Known 1,Known 9,Known 3],[Known 2,Known 8,Known 3],... 4 solutions total]
[[Known 8,Known 9,Known 3],[Known 9,Known 8,Known 3]]
[[Known 1,Known 7,Known 8,Known 9],[Known 1,Known 7,Known 9,Known 8],...]
      last one had 144 solutions total.
--}



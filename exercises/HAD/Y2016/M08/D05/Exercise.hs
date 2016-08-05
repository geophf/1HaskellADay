module Y2016.M08.D05.Exercise where

{--
So, yesterday, we were able to constrain unknowns and, accidentally, narrow
the solution set for a sum-solver to a much smaller count. Yay!

Behind the scenes, the intent was that when two summers cross, we want the
result to reflect in both solution sets. We didn't do this yesterday. Let's
do this.
--}

import Control.Arrow ((&&&))

import Y2016.M08.D04.Exercise

{--
Using the Cell-structure from yesterday, create a Grid-structure that, when
sent to a solver, solves the entire grid. For example:

column:         a      b     c     d      e

             sum 16 sum 4 sum 25 sum 17 sum 4
row
 1    sum 13:   _      _     _      
 2    sum 20:   _      _     _ 
 3    sum 13:                _      _     _
 4    sum 20:                _      _     _


The above (tries to) describe pictorially:

two rows of three cells each,
         row 1 sums to 13,
         row 2 sums to 20, ... etc

three colums: column a, 2 cells, sums to 16
              column b, 2 cells, sums to 4
              column c, 4 cells, sums to 25, ... etc

HMMM! What is the structure of the Grid?

Is it this:

data Grid = [Cell]  -- NOPE!

No, because that does not capture the interrelatedness of the schema.

We need, somehow to say that the grid has a set of schema-rows (and columns)
and also that row 1 connects at cell 1 with column a at cell 1, etc

AND ALSO to make that work with a solver.

Tall order, geophf!

Good thing I'm so tall!

ONE approach is to not worry about the layout at all in the Grid and just
have the crossing points identified. In other words: we don't care if the
schema is in a row or a column, but we do care where the schema cross:
--}

import Data.Map (Map)
import qualified Data.Map as Map

type Idx = Int
type Pos = Int
type Total = Int
type Row = (Total, Schema)
type CrossingPoint = ((Idx, Pos), (Idx, Pos))

{--
CrossingPoint says, e.g. from above Row 1, Position 0 crosses 
                                 at Row 3, Position 0

(That is if row 1 is idx 1, row 2, idx 2, col a, idx 3, col b, idx 4, ... etc)

So with that we can have a grid type:
--}

data Grid = Grd (Map Idx Row) [CrossingPoint]
   deriving Show

-- and our materialized grid is:

unc :: Int -> Schema
unc = (`replicate` Unconstrained)

con :: [Int] -> Schema
con = uncurry replicate . (length &&& ConstrainedTo)

four :: Row
four = (4, con [1,3])

kakuro :: Grid
kakuro = Grd (Map.fromList [(1, (13, unc 3)), (2, (20, unc 3)),
                                                    -- rows 1 and 2
                            (3, (16, con [7,9])), (4, four), (5, (25, unc 4)),
                                                    -- columns a,b,c
                            (6, (13, unc 3)), (7, (20, unc 3)),
                                                    -- rows 3 and 4
                            (8, (17, con [8,9])), (9, four)]) 
                                                    -- columns d,e

             [((1,0),(3,0)), ((1,1),(4,0)), ((1,2),(5,0)),   -- row 1 xings
              ((2,0),(3,1)), ((2,1),(4,1)), ((2,2),(5,1)),   -- row 2 xings
              ((6,0),(5,2)), ((6,1),(8,0)), ((6,2),(9,0)),   -- row 3 xings
              ((7,0),(5,3)), ((7,1),(8,1)), ((7,2),(9,1))]   -- row 4 xings

-- Note that I added the lightest of constraints. Do you want to constrain
-- the grid further by redeclaring it?

kakuroSolver :: Grid -> [Map Idx Row]
kakuroSolver = undefined

-- Hint: solve each schema, but then further refine the solution sets by
-- imposing the equality constraints of the crossing points across the schema

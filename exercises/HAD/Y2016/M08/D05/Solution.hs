module Y2016.M08.D05.Solution where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Y2016.M08.D04.Solution

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

kakuroSolver :: Grid -> [Map Idx Schema]
kakuroSolver (Grd rows xing) = 
   mapM (uncurry (flip solver)) (Map.elems rows) >>= \schemata ->
   let solvedmap = Map.fromList (zip [1..] schemata) in
   guardEach xing solvedmap >> return solvedmap

guardEach :: MonadPlus m => [CrossingPoint] -> Map Idx Schema -> m ()
guardEach [] _ = return ()
guardEach ((a,b):rest) m = guard (idx m a == idx m b) >> guardEach rest m

idx :: Map Idx Schema -> (Idx, Int) -> Cell
idx m (x,y) = m Map.! x !! y

-- ugh, does it solve the problem eventually? Maybe. I got bored waiting for
-- the answer and solved it myself first, and still the solution hasn't returned

-- Solution:

-- 7  1  5
-- 9  3  8
--       3  9  1
--       9  8  3

-- What are faster/better ways to approach this solution?
-- INTERESTING EXERCISE FOR THE READER ... OR: http://lpaste.net/113394

module Y2016.M09.D15.Solution where

import Control.Monad (guard)
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- the below import is available from the 1HaskellADay git repository

import Control.List (takeout)
import Data.Matrix

{--
Today's Haskell exercise is neither sudoku nor magic squares. We're just working
with one square, only, and arranging the number 1 through 9 in those squares.

Simple enough.

Oh, there's a few constraints.

So, one way to arrange the numbers 1 through 9 in a 3x3 matrix is as follows:
--}

threeBy :: Matrix Int
threeBy = fromLists (take 3 (counting [1,2,3]))

counting :: [Int] -> [[Int]]
counting = (map . (+) . length) >>= iterate  -- via joomy @cattheory

{--
*Y2016.M09.D15.Exercise> pprint threeBy
Matrix 3x3
| 1 2 3 |
| 4 5 6 |
| 7 8 9 |
--}

-- But today's problem is a mite harder than threeBy. Here are the constraints:
-- 1. 1 is two squares directly right of 7
-- 2. 2 is two squares directly above 8
-- 3. 3 is two squares directly left of 9
-- 4. 4 is two squares directly below 3
-- 5. 5 is not in the center square

-- create a schema for the constraints so that the constraints hold and the
-- numbers 1 - 9 are arranged in a 3x3 matrix.

-- We see that 6 is unconstrainted
-- We see that 3 is 'doubly constrained'

type Val = Int   -- the value the cell contains
type Idx = Int   -- the index of the Row or the Column

data Col = Col Val Idx
   deriving Show

data Row = Row Val Idx
   deriving Show

data Constraint = SameRow Val Val Col Col
                | SameCol Val Val Row Row
                | NotCenter Val
   deriving Show

constraints :: [Constraint]
constraints = [SameRow 1 7 (Col 1 3) (Col 7 1),
               SameCol 2 8 (Row 2 1) (Row 8 3),
               SameRow 3 9 (Col 3 1) (Col 9 3),
               SameCol 4 3 (Row 4 3) (Row 3 1),
               NotCenter 5]

-- we do the constrain-then-generate approach to solving this puzzle

-- for a constraint, c, we pick indices for the values constrained

type CellIdx = (Idx, Idx)
type ValMap = Map Val CellIdx
type Result = [(ValMap, [(CellIdx, Val)], [CellIdx])]

-- we need to take out an index iff we haven't already assigned the value

takeoutOr :: ValMap -> Val -> [CellIdx] -> [(CellIdx, [CellIdx])]
takeoutOr vm v idxs =
   case Map.lookup v vm of
      Nothing -> takeout idxs
      Just x  -> return (x, idxs)

pick :: ValMap -> [CellIdx] -> Constraint -> Result
pick ctx indices (SameRow v1 v2 (Col _ c1) (Col _ c2)) =
   takeoutOr ctx v1 indices >>= \((a,b), rest) ->
   guard (b == c1) >>
   takeoutOr ctx v2 rest >>= \((c,d), rem) ->
   guard (a == c) >>
   guard (d == c2) >>
   let newmap = Map.insert v1 (a,b) (Map.insert v2 (c,d) ctx) in
   return (newmap, [((a,b), v1), ((c,d), v2)], rem)
pick ctx indices (NotCenter v) =
   takeoutOr ctx v indices >>= \(idx, rest) -> guard (idx /= centre) >>
   return (Map.insert v idx ctx, [(idx, v)], rest)
pick ctx indices (SameCol v1 v2 (Row _ r1) (Row _ r2)) =
   takeoutOr ctx v1 indices >>= \((a,b), rest) ->
   guard (a == r1) >>
   takeoutOr ctx v2 rest >>= \((c,d),rem) ->
   guard (b == d) >>
   guard (c == r2) >>
   let newmap = Map.insert v1 (a,b) (Map.insert v2 (c,d) ctx) in
   return (newmap, [((a,b),v1),((c,d),v2)], rem)

centre :: (Idx, Idx)
centre = (2,2)

picks :: ValMap -> [CellIdx] -> [Constraint] -> Result
picks ctx idxs [] = [(ctx, [], idxs)]
picks ctx idxs (h:t) = pick ctx idxs h >>= \(newctx, ans, rest) ->
   picks newctx rest t >>= \(ctxn, anss, rem) ->
   return (ctxn, ans ++ anss, rem)

constrainedMatrix :: [Constraint] -> Set Int -> [Matrix Int]
constrainedMatrix constraints nums =
   let i3 = matrix (identity 3) in
   picks Map.empty (indices i3) constraints >>= \(_, assigned, rest) ->
   return (M (array (bounds i3) (assigned ++

-- Now, the remaining unconstrained values are assigned to the remaining indices

   zip rest (Set.toList (foldr removeVals nums constraints)))))

removeVals :: Constraint -> Set Val -> Set Val
removeVals (NotCenter v) = Set.delete v
removeVals (SameRow v1 v2 _ _) = Set.delete v1 . Set.delete v2
removeVals (SameCol v1 v2 _ _) = Set.delete v1 . Set.delete v2

-- so constraintedMatrix guards [1..9] gives a properly arrayed matrix

{--
*Y2016.M09.D15.Solution> mapM_ pprint . constrainedMatrix constraints $ Set.fromList [1..9]
Matrix 3x3
| 3 2 9 |
| 7 6 1 |
| 4 8 5 |
--}

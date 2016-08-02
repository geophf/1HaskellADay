module Y2016.M08.D01.Solution where

import Control.List (choose, permute)
import Control.Logic.Frege (assert)

summer :: Int -> Int -> [[Int]]
summer result slotCount =
   choose slotCount [1..9] >>= assert ((== result) . sum) >>= permute

-- Note the return value: there may be multiple solutions.
-- Note also [1,2,3] for the sum of 6 is a different value than [3,2,1]

sumThese :: [(Int, Int)]
sumThese = [(4,2), (10,4), (17,2), (12,2), (13,4)]

-- Answer (for (4,2)) is: [[1,3],[3,1]]

{--
*Y2016.M08.D01.Exercise> summer 4 2 ~> [[1,3],[3,1]]

and, generally:

*Y2016.M08.D01.Exercise> mapM_ (print . uncurry summer) sumThese ~>
[[1,3],[3,1]]
[[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],...
[[8,9],[9,8]]
[[3,9],[9,3],[4,8],[8,4],[5,7],[7,5]]
[[1,2,3,7],[1,2,7,3],[1,3,2,7],[1,3,7,2],[1,7,2,3],...
--}

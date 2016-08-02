module Y2016.M08.D02.Solution where

import Data.List ((\\))
import Data.Monoid

import Control.Logic.Frege (assert)

-- This is actually rather nicely solved by QBits

import Data.QBit

type Digit = Sum Int
type Schema = [QBit Digit]
type Schemata = [Schema]

scheme :: Sum Int -> Schema -> Schemata
scheme total schema = 
-- first we must remove all bound values from the pool
   let pool  = map Sum [1..9]
       taken = map extract (filter observed schema)
-- then we draw the numbers from that pool to get our sum
   in  draws schema (pool \\ taken) >>= \(res, rem) ->
       assert ((== total) . sum . map extract) res

-- Solve the following:

schProblems :: [(Sum Int, Schema)]
schProblems = [(Sum 4, [free, Observed (Sum 1)]),              -- 1
               (Sum 23, [free, free, Observed (Sum 1), free]), -- 2
               (Sum 11, [free, free, Observed (Sum 1), free]), -- 3
               (Sum 23, [free, free, free]),                   -- 4
               (Sum 20, [free, free, free, free, free])]       -- 5

{--
The solution for 1 should be [[Known (Sum 3), Known (Sum 1)]]

*Y2016.M08.D02.Solution> scheme (Sum 4) [free, Observed (Sum 1)] ~>
[[Sum {getSum = 3},Sum {getSum = 1}]]

TA-DAAA!

*Y2016.M08.D02.Solution> mapM_ (print . map (map (getSum . extract)) . uncurry scheme) schProblems ~>
[[3,1]]
[[5,8,1,9],[5,9,1,8],[6,7,1,9],[6,9,1,7],[7,6,1,9],...
[[2,3,1,5],[2,5,1,3],[3,2,1,5],[3,5,1,2],[5,2,1,3],[5,3,1,2]]
[[6,8,9],[6,9,8],[8,6,9],[8,9,6],[9,6,8],[9,8,6]]
[[1,2,3,5,9],[1,2,3,6,8],[1,2,3,8,6],[1,2,3,9,5],[1,2,4,5,8],...

WOOT!
--}

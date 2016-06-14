module Y2016.M06.D13.Solution where

import Control.Arrow ((&&&), second)
import Data.Array (range, bounds, assocs, listArray)

import Control.Logic.Frege (adjoin)
import Data.Matrix

{-- Determining the determinant.

So, the determinant is for a square matrix, the + - + - ... of

Row1 cells * determinant of matrices made by all below cells not in that
row nor column. The matrices so formed are square. The determinant of a
unit matrix is the cell value.

Sounds recurrant.
--}

determinant :: Num a => Matrix a -> a
determinant = uncurry det . (id &&& cpnrows)

det :: Num a => Matrix a -> [[a]] -> a
det _ [[ans]] = ans -- determinant of a 1x1 matrix is its sole element
det mat (row1:rows) =
   sum (zipWith3 product3 (cycle [1, -1]) row1 
                          (map determinant (excludedSubMatrices mat)))

product3 :: Num a => a -> a -> a -> a
product3 a b c = a * b * c

excludedSubMatrices :: Matrix a -> [Matrix a]

-- setup for det: factors a matrix into matrices that are not in each of the
-- first row's columns

excludedSubMatrices mat =
   let newbnd   = second (adjoin pred) (dims mat)
       lowmat   = filter ((/= 1) . fst . fst) (assocs $ matrix mat)
       excludes = range (1, snd . snd $ dims mat)
   in  map (M . listArray newbnd . flip subMatrixOf lowmat) excludes 

subMatrixOf :: Int -> [((Int, Int), a)] -> [a]
subMatrixOf excludeCol = map snd . filter ((/= excludeCol) . snd . fst)

-- With the above defined, what are the determinants of the below matrices?

ex1, ex2, ex10 :: Matrix Float

ex1  = fromLists [[2,5], [1, -3]]
ex2  = fromLists [[3,-5],[2,1]]
ex10 = fromLists [[2,-1,0], [3, -5, 2], [1,4,-2]]

-- exercises are from mathopolis.com

{--
*Y2016.M06.D13.Solution> determinant ex1 ~> -11.0
*Y2016.M06.D13.Solution> determinant ex2 ~> -13.0
*Y2016.M06.D13.Solution> determinant ex10 ~> -4.0
*Y2016.M06.D13.Solution> determinant (fromLists [[3,0,-1],[2,-5,4],[-3,1,3]]) ~> -44
*Y2016.M06.D13.Solution> determinant (fromLists [[2,0,-1],[3,5,2],[-4,1,4]]) ~> 13

Rolling determinant into Data.Matrix
--}

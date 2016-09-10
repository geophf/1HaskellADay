{-# LANGUAGE TupleSections #-}

module Y2016.M09.D09.Solution where

import Control.Arrow (first)
import Control.Monad (guard, (>=>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

-- below import available from 1HaskellADay git repository

import Control.List (takeout)

{--
Now for something completely different ...

So some related cryptoarithmetic:
--}

sums :: [(String, String, Int)]
sums = [("FAT", "CAT", 904), ("STORE","CAKE",60836), ("EACH", "ROSE", 7839)]

-- solve these sums

-- Okay, first we need to assign a letter to a value

assign :: Map Char Int -> Char -> [Int] -> [((Char, Int), [Int])]
assign ctx ch domain =
   maybe (fmap (first (ch,)) (takeout domain)) (pure . (,domain) . (ch,))
         (Map.lookup ch ctx)

-- we do that for each of the letters. 

assigns :: Map Char Int -> String -> [Int] -> [([(Char, Int)], [Int])]
assigns _ [] = pure . ([],)
assigns ctx (ch:s) =
   assign ctx ch >=> \(a,b) -> assigns ctx s b >>= return . first (a:)

{--
*Y2016.M09.D09.Solution> let ans = assigns Map.empty "FAT" [0..9]
*Y2016.M09.D09.Solution> head ans ~> ([('F',0),('A',1),('T',2)],[3,4,5,6,7,8,9])
*Y2016.M09.D09.Solution> last ans ~> ([('F',9),('A',8),('T',7)],[0,1,2,3,4,5,6])
*Y2016.M09.D09.Solution> length ans ~> 720
--}

-- Okay, we have that, so let's convert that to a map. With arrows, this is
-- easy, right: map (first Map.fromList)

-- Now we convert a word into a number:

numfrom :: Map Char Int -> String -> Int
numfrom ctx = foldl (\tots -> (10 * tots +) . (ctx Map.!)) 0

{--
*Y2016.M09.D09.Solution> let fats = map (first (flip numfrom "FAT" . Map.fromList)) ans
*Y2016.M09.D09.Solution> head fats ~> (210,[3,4,5,6,7,8,9])
*Y2016.M09.D09.Solution> last fats ~> (789,[0,1,2,3,4,5,6])
--}

solver :: (String, String, Int) -> Map Char Int -> [Int] -> [(Map Char Int, [Int])]
solver (a,b,c) ctx domain =
   assigns ctx a domain                 >>= \(asol, rest)   -> 
   let amap = merge ctx asol
       na = numfrom amap a       in
   guard (na < c)                       >>
   assigns amap b rest                  >>= \(bsol, subdom) ->
   let newmap = merge amap bsol  in
   guard (na + numfrom newmap b == c)   >>
   return (newmap, subdom)

merge :: Ord a => Map a b -> [(a,b)] -> Map a b
merge m = Map.union m . Map.fromList

-- solving all three will map the letter to the digits [0..9]

{--
*Y2016.M09.D09.Solution> let solves = solver Map.empty [0..9] (head sums)
*Y2016.M09.D09.Solution> length solves ~> 10
*Y2016.M09.D09.Solution> head solves
(fromList [('A',5),('C',8),('F',0),('T',2)],[1,3,4,6,7,9])

so:
--}

solveAll :: [(String, String, Int)] -> Map Char Int -> [Int] -> [Map Char Int]
solveAll [] ctx = const (pure ctx)
solveAll (e:qs) ctx = solver e ctx >=> uncurry (solveAll qs) . first (Map.union ctx)

{--
*Y2016.M09.D09.Solution> solveAll sums Map.empty [0..9] ~>
{[('A',0),('C',8),('E',3),('F',1),('H',6),('K',9),('O',7),('R',4),('S',5),('T',2)]}
--}

-- arrange the character in their digit-value order. What word do you get?

arrange :: Map Char Int -> String
arrange = map fst . sortBy (compare `on` snd) . Map.toList

-- *Y2016.M09.D09.Solution> arrange (head it) ~> "AFTERSHOCK"

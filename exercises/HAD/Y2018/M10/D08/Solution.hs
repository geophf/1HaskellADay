module Y2018.M10.D08.Solution where

{--
Happy Columbus Day and Indiginous People's Day!

Today, we are given a set of ids for today and we will generate a subset of
those ids, given a set of indices. You can look at this as a random draw for
screening, perhaps. Or something else, if you'd like.
--}

import Data.Array

exDir, idsToday :: FilePath
exDir = "Y2018/M10/D08/"
idsToday = "ids-today.txt"

picks :: [Int]
picks = [4, 6, 7, 9, 10, 19, 23, 24, 29, 32, 33, 34, 35, 37, 39, 46, 54, 55, 
         58, 59, 63, 64, 67, 73, 78, 79, 89, 94, 98, 99]

type ID = String

picked :: FilePath -> [Int] -> IO [ID]
picked idFile picks = readFile idFile >>= 
   return . flip map picks . (!) . array (0,99) . zip [0..] . lines

-- We convert the list to an array to eliminate multiple linear traversals
-- while indexing into the ids to get the selected members.

{--
>>> take 3 <$> picked (exDir ++ idsToday) picks
["thXKM68kkXSE74WJWGQxp8LV","n2NJbdKH9dTbcfJWvm2R49mZ","rVEQCZCJJM2ywMgLsRha3bvb"]
--}

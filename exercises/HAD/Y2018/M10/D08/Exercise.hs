module Y2018.M10.D08.Exercise where

{--
Happy Columbus Day and Indinginous People's Day!

Today, we are given a set of ids for today and we will generate a subset of
those ids, given a set of indices. You can look at this as a random draw for
screening, perhaps. Or something else, if you'd like.
--}

exDir, idsToday :: FilePath
exDir = "Y2018/M10/D08/"
idsToday = "ids-today.txt"

picks :: [Int]
picks = [4, 6, 7, 9, 10, 19, 23, 24, 29, 32, 33, 34, 35, 37, 39, 46, 54, 55, 
         58, 59, 63, 64, 67, 73, 78, 79, 89, 94, 98, 99]

type ID = String

picked :: FilePath -> [Int] -> IO [ID]
picked idFile picks = undefined

-- Have at it!

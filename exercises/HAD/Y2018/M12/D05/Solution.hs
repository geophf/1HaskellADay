{-# LANGUAGE TupleSections, ViewPatterns #-}

module Y2018.M12.D05.Solution where

import Data.Array
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Set (Set)
import qualified Data.Set as Set

import System.Random

import Y2018.M12.D03.Solution

-- So you have the 5-letter palindromes from yesterday's exercise:

palindromes :: [String]
palindromes = ["AJAJA","ALALA","ALULA","ANANA","ARARA","CIVIC","KAYAK","KAZAK",
               "KELEK","LEMEL","LEVEL","MADAM","MESEM","MINIM","RADAR","REFER",
               "REVER","ROTOR","SAMAS","SERES","SIRIS","TEBET","TENET","ULULU",
               "YARAY"]

-- today, choose one of those words, and, like Monday's exercise, write a
-- 30 x 30 grid of random letters of that word, then, find that word in that
-- grid, you WILD HASKELL PUZZLE-MAKER, YOU!
 
-- (declarations for Grid, grid, printGrid imported above)

gridFromWord :: Size -> String -> IO Grid
gridFromWord sz word = array ((1,1),(sz,sz)) . concat
                   <$> mapM (flip (rowWord word) sz) [1 .. sz]

rowWord :: String -> Row -> Size -> IO [((Int,Int), Char)]
rowWord wrd r sz = mapM (\c -> indexed wrd >>= return . ((r,c),)) [1 .. sz]

indexed :: String -> IO Char
indexed wrd = let word = Set.fromList (map toUpper wrd) in
              (toArr word !) <$> randomRIO (1,length word)

toArr :: Set Char -> Array Int Char
toArr wrd = listArray (1,length wrd) (Set.toList wrd)

{--
>>> gridFromWord 10 "radar" >>= printGrid 
 R A D D A A A A R A
 R A A A R R R A A D
 A A R A D D D A D D
 A A R R R R D D A R
 D R R R D A R D D R
 A R R A A A D D D A
 A A R D A R D R D D
 A R D A A R R R A D
 D A D R A D R R D D
 R D D D A R D A A R
--}

hasWordInRowOrColumn :: Grid -> String -> Bool
hasWordInRowOrColumn grid (map toUpper -> word) =
   any (isInfixOf word) (rows grid ++ cols grid)

hasWordInDiagonals :: Grid -> String -> Bool
hasWordInDiagonals grid (map toUpper -> word) =
   any (isInfixOf word) (lDiags grid ++ rDiags grid)

wordCount :: Grid -> String -> Int
wordCount g (map toUpper -> word) =
   sum (map (countWord word) (rows g ++ cols g ++ lDiags g ++ rDiags g))

countWord :: String -> String -> Int
countWord = cw 0

cw :: Int -> String -> String -> Int
cw ans word "" = ans
cw acc word str = let substr = str `minus` word in
   if substr == "" then acc else cw (succ acc) word substr

-- BONUS -----------------------------------------------------------------

-- For any five-letter word, generate a grid of its letters, then find the
-- number of times that word appears in that grid.

fiveLetterGrid :: Size -> String -> IO Grid
fiveLetterGrid = gridFromWord

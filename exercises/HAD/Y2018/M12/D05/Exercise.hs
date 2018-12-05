module Y2018.M12.D05.Exercise where

import Y2018.M12.D03.Exercise

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

hasWordInRowOrColumn :: Grid -> Bool
hasWordInRowOrColumn grid = undefined

hasWordInDiagonals :: Grid -> Bool
hasWordInDiagonals grid = undefined

wordCount :: Grid -> Int
wordCount grid = undefined
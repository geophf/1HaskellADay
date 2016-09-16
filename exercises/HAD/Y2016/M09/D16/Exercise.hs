module Y2016.M09.D16.Exercise where

-- below module is available from 1HaskellADay git repository

import Data.Matrix

{-- 
Here's a Sudoku with a word-twist. 

The below puzzle contains the word BINOCULAR in each row, column, and in each
nonent (9 cells) with centers at
--}

nonentCenters :: [(Int, Int)]
nonentCenters = [(r,c) | r <- centers, c <- centers]
   where centers = [2,5,8]

-- So, a nonent is the cells with a nonent-center (above) and the eight cells
-- encircling that nonent-center. Define a nonent algorithmically:

nonent :: (Int, Int) -> [(Int, Int)]
nonent center = undefined

     -- gives the cell indices of the nonent, including the given center index

-- Okay, so here is the BINOCULAR sudoku. 

data Letter = Ltr Char
   deriving (Eq, Ord)

instance Show Letter where
   show (Ltr l) = pure l

binocularPuzzle :: Matrix Letter
binocularPuzzle =
   fromLists 
     (map (map Ltr) ["AU O  C  ","  RI    U"," I  U NB ","  UR I C ","IC     RN",
                    " N U CL  "," AI O    ","B    UI  ","  C  L UO"])

{--
*Y2016.M09.D16.Exercise> pprint binocularPuzzle ~>
Matrix 9x9
| A U   O     C     |
|     R I         U |
|   I     U   N B   |
|     U R   I   C   |
| I C           R N |
|   N   U   C L     |
|   A I   O         |
| B         U I     |
|     C     L   U O |

Solve it by replacing the spaces with the missing letters from the word
--}

word :: [Letter]
word = map Ltr "BINOCULAR"

-- *Y2016.M09.D16.Exercise> word ~> [B,I,N,O,C,U,L,A,R]

-- given the standard rules of sudoku: no letter repeats in a row, column, nor
-- nonent

{-- BONUS -----------------------------------------------------------------

List all 9 letter words with unique letters. BRAGGART isn't one of them, because
it has repeated 'G's.

Hint: use, e.g. on macs: /usr/share/dict/words or some other lexicon to help.
--}

wordsofunique9letters :: [String] -> [String]
wordsofunique9letters = undefined

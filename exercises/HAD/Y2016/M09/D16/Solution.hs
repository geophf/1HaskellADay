module Y2016.M09.D16.Solution where

import Control.Arrow ((&&&), (>>>))
import Control.Monad (guard)
import Data.Array ((//))
import Data.List ((\\), delete)
import Data.Monoid

-- below module is available from 1HaskellADay git repository

import Control.List (takeout)
import Control.Logic.Frege (assert, adjoin)
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

nonent' :: (Int, Int) -> [(Int, Int)]
nonent' (r,c) = [(r-r1,c-c1) | r1 <- steps, c1 <- steps]
   where steps = [-1,0,1]

-- *Y2016.M09.D16.Solution> nonent (2,2) ~> 
-- [(3,3),(3,2),(3,1),(2,3),(2,2),(2,1),(1,3),(1,2),(1,1)]

-- Okay, so here is the BINOCULAR sudoku. 

data Letter = Ltr Char

instance Show Letter where
   show (Ltr l) = pure l

instance Monoid Letter where
   mempty = Ltr ' '
   mappend (Ltr ' ') a = a
   mappend a _         = a

binocularPuzzle :: Matrix Letter
binocularPuzzle =
   fromLists 
     (map (map Ltr) ["AU O  C  ","  RI    U"," I  U NB ",
                     "  UR I C ","IC     RN"," N U CL  ",
                     " AI O    ","B    UI  ","  C  L UO"])

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

-- we can solve this with an unusual variant on the Eq instance...
-- And this is why this is a matrix of Letters, not Chars...

instance Eq Letter where
   Ltr ' ' == _       = True
   _ == Ltr ' '       = True
   Ltr a   == Ltr b   = a == b
   Ltr ' ' /= _       = True
   _       /= Ltr ' ' = True
   Ltr a   /= Ltr b   = a /= b

lettersOf :: [Letter] -> [Letter]
lettersOf [] = []
lettersOf (Ltr ' ':rest) = lettersOf rest
lettersOf (l:rest) = l:lettersOf rest

nonentAt :: (Int, Int) -> Int
nonentAt (r,c) = (pred r `div` 3) * 3 + (pred c `div` 3 + 1)

nonentCenterFrom :: Int -> (Int, Int)
nonentCenterFrom = adjoin ((+2) . (*3)) . ((`div` 3) &&& (`mod` 3)) . pred

-- *Y2016.M09.D16.Solution> map nonentCenterFrom [1..9]
-- [(2,2),(2,5),(2,8),(5,2),(5,5),(5,8),(8,2),(8,5),(8,8)]

nonentIndices :: Int -> [(Int, Int)]
nonentIndices = nonent' . nonentCenterFrom

{-- 
*Y2016.M09.D16.Solution> nonentIndices 1
[(3,3),(3,2),(3,1),(2,3),(2,2),(2,1),(1,3),(1,2),(1,1)]
*Y2016.M09.D16.Solution> nonentIndices 9
[(9,9),(9,8),(9,7),(8,9),(8,8),(8,7),(7,9),(7,8),(7,7)]
--}

type L = Letter

solver :: [L] -> Matrix L -> [Matrix L]
solver run = uncurry (solveEachRow run 1) . (id &&& rows)

solveEachRow :: [L] -> Int -> Matrix L -> [[L]] -> [Matrix L]
solveEachRow _ _ m [] = [m]
solveEachRow run nrow mat@(M m) (r:ows) =
   solveRow (run \\ lettersOf r) nrow 1 mat r >>= \row ->
   solveEachRow run (succ nrow) (M (m // zip [(nrow, c) | c <- [1..9]] row)) ows

solveRow :: [L] -> Int -> Int -> Matrix L -> [L] -> [[L]]
solveRow _ _ _ _ [] = [[]]
solveRow run nrow ncol mat (r:ow) =
   let colltrs  = lettersOf (col ncol mat)
       none     = nonentAt (nrow, ncol)
       nonltrs  = lettersOf (map (cell mat) (nonentIndices none))
       verboten = colltrs ++ nonltrs in
   solveCell (run \\ verboten) r >>= \(c, _) ->
   solveRow (delete c run) nrow (succ ncol) mat ow >>= return . (c:)

solveCell :: [L] -> L -> [(L, [L])]
solveCell run (Ltr ' ') = takeout run
solveCell run l = pure (l, run)

{-- 
instead of all this, we remove the letters of the row and the column before we
even select a letter

   guard (ch1 == ch) >>
   guard (ch1 `notElem` lettersOf (drop ncol (row nrow mat))) >>
   guard (ch1 `notElem` lettersOf (drop nrow (col ncol mat))) >>

   -- must add guard for nonent --

   return ans

so, note in the definitions above, we do not generate-then-test with guards
but instead generate, then roll each row into the basis matrix and from that
basis derive our solution: much faster, and importantly, correct!

*Y2016.M09.D16.Solution> let sol = solver word binocularPuzzle 
*Y2016.M09.D16.Solution> pprint (head sol)
Matrix 9x9
| A U N O B R C I L |
| L B R I C N O A U |
| C I O L U A N B R |
| O L U R N I A C B |
| I C A B L O U R N |
| R N B U A C L O I |
| U A I N O B R L C |
| B O L C R U I N A |
| N R C A I L B U O |
*Y2016.M09.D16.Solution> length sol ~> 1
--}

{-- BONUS -----------------------------------------------------------------

List all 9 letter words with unique letters. BRAGGART isn't one of them, because
it has repeated 'G's.

Hint: use, e.g. on macs: /usr/share/dict/words or some other lexicon to help.
--}

wordsofunique9letters :: [String] -> [String]
wordsofunique9letters lexicon =
   lexicon >>= assert ((== 9) . length &&& allDiff >>> uncurry (&&))

allDiff :: Eq a => [a] -> Bool
allDiff [] = True
allDiff (h:t) = all (/= h) t && allDiff t

{--
*Y2016.M09.D16.Solution> fmap (wordsofunique9letters . lines) $ readFile "/usr/share/dict/words" ~> nineltrs
["abduction","abjection",...,"zygotaxis","zymogenic"]
*Y2016.M09.D16.Solution> length nineltrs ~> 3960

Wow! Quite a few words, actually.
--}

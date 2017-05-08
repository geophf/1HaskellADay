module Y2017.M05.D08.Solution where

import Control.Monad (guard)

-- below imports available via 1HaskellADay git repository

import Data.QBit
import Data.Numeral.QBits

{--
So, we're back on the chain gang.

No: we're back to solving cryptarithmic puzzles via the Mensa Genius Quiz-a-Day
Book by Dr. Abbie F. Salny, et al.

"In this addition alphametic, you can replace each letter with a number (the
same number for each letter) to find a correct arithmetic solution.

(Hint: A = 1)


      H A N G
      H A N G
    + H A N G
    ---------
    G A N G S
--}

hangGangs :: Nums -> Nums -> [(Nums, Nums)]
hangGangs hang@(N h) _ =
   draws h [0..9] >>= \(h', rest) ->
   draw free rest >>= \(s, _)     ->
   let num = asNum (N h')
       [_, a, n, g] = h'
       gangs = [g, a, n, g, s]
       ans = asNum (N gangs) in
   guard (3 * num == ans) >>
   return (N h', N gangs)

{--
>>> hang = N [constrain notZero, Observed 1, free, constrain notZero]
>>> hangGangs hang (N [])
[(7142,21426)]
--}

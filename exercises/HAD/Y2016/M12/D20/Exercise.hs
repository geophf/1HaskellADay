module Y2016.M12.D20.Exercise where

-- below import available from 1HaskellADay git repository

import Data.SymbolTable
import Data.SymbolTable.Compiler

import Data.SAIPE.USStates

{--
So, yesterday, as you see from the import above, we enumerated the US States!

YAY! *throws confetti

NOW! Using the same data-set, stored at:

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

let's enumerate the US Counties.

A slight problem: the SymbolTable-type works with strings, SO you can enumerate
the strings, sure.

(I think any Ord-type should work, but hey.)

('but hey' is a perfect counter-argument to anything. Try it sometime.)

Well you can do that. No problem. But how do you associate an atomic County
symbol with its State?

Today's Haskell problem.

1) create a complied symbol table of Data.SAIPE.USCounties, enumerated and
indexible.
--}

usCountySymbols :: FilePath -> FilePath -> IO ()
usCountySymbols gzipSAIPEdata modul = undefined

data USCounty = PlaceholderForCompiledUSCountyType

-- see yesterday's exercise for guidance

{--
Now, USState and USCounty and their associations are all deterministic (as we'd
say in Prolog-parlance), so how do we do a lookup for the USState of a USCounty
in a deterministic fashion?
--}

usStateFor :: USCounty -> USState
usStateFor county = undefined

-- 2) define usStateFor as above that guarantees to return a USState value
-- (the correct one, at that) for the input USCounty value.

-- hint: A USCounty's USState is a fact. No doubt.

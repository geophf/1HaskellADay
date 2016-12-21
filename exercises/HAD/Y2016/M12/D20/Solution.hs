module Y2016.M12.D20.Solution where

import Control.Monad.State

-- below import available from 1HaskellADay git repository

import Data.SymbolTable (empty, fromEnumS)
import Data.SymbolTable.Compiler (compile)

import Data.SAIPE.USStates
import Data.SAIPE.USCounties  -- after we bootstrap it here. I know: magic!

import Y2016.M12.D15.Solution

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

usCounties :: [[String]] -> [County]
usCounties = tail
           . concatMap (either (const []) (pure . meld)
                      . stateContext . (!! 3))

-- line2SAIPERow discards StateAbbrv information, so we have to route around
-- a bit to get that info using Lens-hackery

meld :: (County, StateAbbrev) -> String
meld (x, y) = x ++ " (" ++ y ++ ")"

{--
*Y2016.M12.D20.Solution> readSAIPERaw "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" ~> rows
*Y2016.M12.D20.Solution> fmap (take 4 . usCounties) rows ~>
["Baldwin County (AL)","Barbour County (AL)",
 "Bibb County (AL)","Blount County (AL)"]
--}

usCountySymbols :: FilePath -> FilePath -> IO ()
usCountySymbols gzipSAIPEdata modul = readSAIPERaw gzipSAIPEdata             >>=
   compile "USCounty" modul . (`execState` empty) . mapM_ fromEnumS . usCounties

{--
*Y2016.M12.D20.Solution> usCountySymbols "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" "Data.SAIPE.USCounties"

(added USCounties module to this directory for reference)

(quit ghci, then:)

geophf:HAD geophf$ ghci Data/SAIPE/USCounties.hs ( ... that took a bit)
*Data.SAIPE.USCounties> S23 ~> DeKalb County (AL)
*Data.SAIPE.USCounties> read "Middlesex County (CT)" :: USCounty ~>
Middlesex County (CT)
*Data.SAIPE.USCounties> fromEnum it ~> 1910

WOOT!

data USCounty = PlaceholderForCompiledUSCountyType

-- see yesterday's exercise for guidance

Now, USState and USCounty and their associations are all deterministic (as we'd
say in Prolog-parlance), so how do we do a lookup for the USState of a USCounty
in a deterministic fashion?

usStateFor :: USCounty -> USState
usStateFor county = undefined

Okay, here's the deal-i-o, joe. Since USCounty is known, and USState is known
(unlike their String counterparts, which may not be known, then we can simply
write a compiler that writes each known quantity. AND, we have this known
quantity (and much more, actually) from the parsing exercise when we converted
the raw file read to SAIPERow information.

-- 2) define usStateFor as above that guarantees to return a USState value
-- (the correct one, at that) for the input USCounty value.

-- hint: A USCounty's USState is a fact. No doubt.

header :: String
header = "module Data.SAIPE.StatesCounties where"

imports :: [String]
imports = map ("import Data.SAIPE." ++) (words "USStates USCounties")

preamble :: String
preamble = "usStateFor :: USCounty -> USState"

fnWriter :: SAIPEData -> [String]
fnWriter = concatMap ...
--}

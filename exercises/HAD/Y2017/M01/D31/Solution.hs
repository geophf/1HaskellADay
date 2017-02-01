{-# LANGUAGE ViewPatterns #-}

module Y2017.M01.D31.Solution where

import Data.Maybe (fromJust)

-- below import available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin)

{--
So, yesterday we read the header file header and were able to extract Julian
dates. Now let's start to look at the Ephemerides data sets for the EMbary,
"Earth/Moon Barycenter," and for Mars. The internal format is (obliquely)
described in the document ascii_format.txt at this directory.

In brief, the ephemeris data is given as

<header>
<coordinate data in Double-precision triples>

Problem one:

Read in 1 double-precision number from the text file. So, we have double-
precision triples. Let's just read in one of them:
--}

samplePoint :: String
samplePoint = "  3.461497957769017000D+07 -9.557376026160824000D+06 -2.490153072986354000D+06"

-- above is a sample 3-dimensional point. Read in each of these and re-represent
-- these values as Haskell numbers

-- hint: (read "3.461497957769017000D+07") :: Double throws this error:
-- *** Exception: Prelude.read: no parse
-- How do you read a double, then?

-- the solution is really obvious and simple: change 'D' to 'E' or 'e'

readDouble :: String -> Double
readDouble = read . map δD2E

δD2E :: Char -> Char
δD2E 'D' = 'E'
δD2E x   = x

-- *Y2017.M01.D31.Solution> readDouble "3.461497957769017000E+07" ~>
-- 3.461497957769017e7

-- Problem two:
-- read in a line and return a Triple:

data Triple = ThreeD { x,y,z :: Double }
   deriving (Eq, Ord, Show)

read3DPoint :: String -> Triple
read3DPoint (map readDouble . words -> [x,y,z]) = ThreeD x y z

{--
*Y2017.M01.D31.Solution> read3DPoint samplePoint ~>
ThreeD {x = 3.461497957769017e7, y = -9557376.026160823, z = -2490153.072986354}
--}

-- Problem three:
-- These data are in a file with other information. Group these data accordingly

data Epoch = Ephem { file :: String, group, ncoefficient :: Int, points :: [Triple] }
   deriving (Eq, Ord, Show)

{--
To do this we need a 'machine' or builder or automata to read in epochs.

The algorithm is this

1. if header, read header and start new epoch (meaning: complete old epoch)
2. if triple, read triple, add to set of triples
3. lather, rinse, repeat until EOF.

So, we need to read in an epoch header:
--}

header :: String -> Maybe (Int, Int)
header str = case words str of
   pair@[grp, coeff] -> Just (adjoin read (grp, coeff))
   _                 -> Nothing

-- now we need to lift the read3DPoint into the Maybe-monad. But carefully:

-- okay, we can read a single triple. How do we know we're done? Easy:
-- 1. we reach EOF; or,
-- 2. header succeeds

triple :: String -> Maybe Triple
triple line = case header line of
   Just _ -> Nothing
   _      -> Just (read3DPoint line)

-- now we need read in a set of triples and return the rest of the file

triples :: [String] -> ([Triple], [String])
triples file@(h:t) = case triple h of
   Just trip -> let (rest, lines) = triples t in (trip:rest, lines)
   Nothing   -> ([], file)
triples []         = ([], [])

{--
*Y2017.M01.D31.Solution> readFile "Y2017/M01/D31/de102/ascm0800-2recs.102" ~> earth
*Y2017.M01.D31.Solution> let l = lines earth ~> length ~> 518
*Y2017.M01.D31.Solution> length (fst $ triples l) ~> 0

No triples as it encounters the header, so:

*Y2017.M01.D31.Solution> let ans@(trips, rest) = triples (tail l)
*Y2017.M01.D31.Solution> (length trips, length rest) ~> (258,259)

and:

*Y2017.M01.D31.Solution> head rest
"     2   773"

pointing right at the next header. Good!
--}

-- So an epoch is the header + the triples with the rest of the file
-- the epochs is an epoch with the epochs of the rest of the file

epochs :: FilePath -> [String] -> [Epoch]
epochs _ [] = []
epochs filename (hdr:rest) =
   let (trips, rem) = triples rest in
   uncurry (Ephem filename) (fromJust $ header hdr) trips:epochs filename rem

{--
*Y2017.M01.D31.Solution> let eps = epochs "Y2017/M01/D31/de102/ascm0800-2recs.102" (lines earth)
*Y2017.M01.D31.Solution> length eps ~> 2
*Y2017.M01.D31.Solution> length (points (head eps)) ~> 258
*Y2017.M01.D31.Solution> length (points (last eps)) ~> 258
--}

readEpochs :: FilePath -> IO [Epoch]
readEpochs file = epochs file . lines <$> readFile file

{-- Questions:
1. How many epoch are in ascm0800-2recs.102 and in ascm1100-2recs.102
   (these files are located under de102/ in this directory)

*Y2017.M01.D31.Solution> readEpochs "Y2017/M01/D31/de102/ascm0800-2recs.102" ~> earthEpochs
*Y2017.M01.D31.Solution> length earthEpochs ~> 2

*Y2017.M01.D31.Solution> readEpochs "Y2017/M01/D31/de102/ascm1100-2recs.102" ~> marseEpochs
*Y2017.M01.D31.Solution> length marsEpochs ~> 2

2. How many Triples are in each epoch for each file? Same number for each
   file, or the number of Triples different for each file?

*Y2017.M01.D31.Solution> map (length . points) earthEpochs ~> [258,258]
*Y2017.M01.D31.Solution> map (length . points) marsEpochs ~> [258,258]

3. What is the z value for the last triple for each epoch?

*Y2017.M01.D31.Solution> map (z . last . points) earthEpochs ~> [0.0,0.0]
*Y2017.M01.D31.Solution> map (z . last . points) marsEpochs ~> [0.0,0.0]
--}

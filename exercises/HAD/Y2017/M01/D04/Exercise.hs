module Y2017.M01.D04.Exercise where

import Codec.Compression.GZip

-- below imports availabe via 1HaskellADay git repository

import Data.SAIPE.USStates
import Graph.ScoreCard

import Y2016.M12.D21.Exercise

{--
Today we're going to go back an relook the SAIPE/poverty data posted at:

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

Today's problem? Read in the SAIPE data and score them, not by USCounty, but
by USState.
--}

readSAIPEUSStateData :: FilePath -> [ScoreCard USState Axes Float]
readSAIPEUSStateData gzipfile = undefined

-- Hint: you may need to accumulate county data? That's one approach. There
-- may be others.

-- Not today, but when we have the USState SAIPE data, we'll also look at
-- USState debt data (per capita/per State) and see if there are correllations.

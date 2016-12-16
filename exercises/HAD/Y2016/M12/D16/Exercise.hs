module Y2016.M12.D16.Exercise where

import Codec.Compression.GZip

-- below module available via 1HaskellADay git repository

import Graph.ScoreCard

{--
Same data set:

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

This time, we're going to be scoring those data, or, at least, starting to look
at how to score these data.

--}

readCensusDataAsScoreCards :: FilePath -> IO [ScoreCard a b c]
readCensusDataAsScoreCards zipFile = undefined

{--
So, what are the types, a, b, and c for the score cards.

Also, make sure to exclude grouping/accumulated (State and US) rows from your 
resulting set of ScoreCards.

Next week, we will look at clustering these data and then looking for 
similarities among ScoreCards within these clusters.
--}

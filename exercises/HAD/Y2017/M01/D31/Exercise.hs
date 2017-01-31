module Y2017.M01.D31.Exercise where

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

parseDouble :: String -> Double
parseDouble = undefined

-- hint: (read "3.461497957769017000D+07") :: Double throws this error:
-- *** Exception: Prelude.read: no parse
-- How do you read a double, then?

-- Problem two:
-- read in a line and return a Triple:

data Triple = ThreeD { x,y,z :: Double }
   deriving (Eq, Ord, Show)

parse3DPoint :: String -> Triple
parse3DPoint = undefined

-- Problem three:
-- These data are in a file with other information. Group these data accordingly

data Epoch = Ephem { file :: String, group, ncoefficient :: Int, points :: [Triple] }
   deriving (Eq, Ord, Show)

readEpochs :: FilePath -> IO [Epoch]
readEpochs = undefined

-- Questions:
-- 1. How many epoch are in ascm0800-2recs.102 and in ascm1100-2recs.102
--    (these files are located under de102/ in this directory)
-- 2. How many Triples are in each epoch for each file? Same number for each
--    file, or the number of Triples different for each file?
-- 3. What is the z value for the last triple for each epoch?

module Y2018.M06.D08.Exercise where

import Data.Time

{--
Okay, we got this here dater.txt file, and we wish to view these columns of
data as rows of data. That is to say: transpose this matrix.

But with a twist, see? Because the date-times are internally separated by
spaces as well, so we need the date-times as date-time units, not as date in
one row and time in another row, because that's all awkward, as real-world dater
is wont to be.

So you have this here dater:
--}

dater, exDir :: FilePath
dater = "dater1.txt"
exDir = "Y2018/M06/D08/"

-- read in the dater and transpose it so I can look at without going cross-eyed

data Dater =
   Dater { id :: Integer, fln :: String, received :: Day, record :: Integer}
      deriving (Eq, Ord, Show)

readDater :: FilePath -> IO [Dater]
readDater file = undefined

{-- BONUS -----------------------------------------------------------------

Dater-analysis.

Now SOME of the daters aren't formatted so friendly-like. Read those in, too,
even, belike. 

--}

cleanThenReadDater :: FilePath -> IO [Dater]
cleanThenReadDater file = undefined

-- here are the other daters:

otherDaters :: [FilePath]
otherDaters = map (("dater" ++) . (++ ".txt")) (words "2 3 4")

-- Now that you have read in these daters, are there any records that are the
-- same across dater-sets? Are there any fln's that are the same?

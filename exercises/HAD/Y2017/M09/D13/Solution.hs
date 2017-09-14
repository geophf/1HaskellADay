module Y2017.M09.D13.Solution where

import Data.Time
import Data.Time.Clock
import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Y2017.M09.D08.Solution

{--
There's something funny about the article titles of the Y2017.M09.D08.Exercise

Check out the title names.

Ring any bells? They did to me. These are funny encodings of Julian dates.

Hear me out.

Let's look at:
--}

sampleTitle :: FileName
sampleTitle = "AP900327-0242.txt"

{--
dateFromJulianDate :: Integer -> Day
dateFromJulianDate jd = undefined

-- hint, you may need to do a bit of research here, as it may require more
-- than the functions provided for by Data.Time.Calendar.Julian

Actually, after looking at this sideways, and realizing something: those
are Gregorian dates, but not Y2K-compliant. SO, this becomes a bit easier
to recode
--}

dateFromString :: String -> UTCTime
dateFromString (y1:y2:m1:m2:d1:d2:_dash:h1:h2:minutes) =
   UTCTime (fromGregorian (1900 + fromIntegral (c y1 y2)) (c m1 m2) (c d1 d2))
           (secondsToDiffTime (mins minutes * 60 + 3600 * c h1 h2))
      where c x y = read [x,y]
            mins (m1:m2:_) = c m1 m2

{--
>>> dateFromString (drop 2 sampleTitle)
1990-03-27 02:42:00 UTC
--}

-- TODAY'S HASKELL EXERCISE -------------------------------------------------

-- Read in the file names at 

articlesDir :: FilePath
articlesDir = "Y2017/M09/D08/articles/b/"

-- and for each article extract the Julian date (by reading in the first 
-- Integer) and return the corresponding Day published for that article:

fileNameToDay :: FilePath -> UTCTime
fileNameToDay = dateFromString . drop 2

{--
>>> fmap (map fileNameToDay . filter (startsWith '.')) $ filesAt articlesDir ""
[1990-03-27,1990-03-27,1990-03-27,1990-03-28,1990-03-28,1990-03-29,1990-03-30,
 1990-03-30,1990-04-01,1990-04-02]

So: question!

ARE these hidden Julian dates?
OR is geophf off his rocker?

I guess we'll never know the answer to some questions in life ...
--}

{-- BONUS -----------------------------------------------------------------

Or, you can use the API call from the US Naval Observatory, e.g.:

http://aa.usno.navy.mil/jdconverter?ID=AA&jd=900327&format=json
--}

myID :: String
myID = "geophf"

-- and as a courtesy, email the Navy with your id and who you are:

navyEmail :: URL
navyEmail = "navobsy_aa-help@navy.mil"

-- they use this to justify their services

jdconverter :: URL
jdconverter = "http://api.usno.navy.mil/jdconverter"

-- call the jdconverter with your id and the jd. What do you get?

-- ... you may get a surprise. Just saying.

{--
>>> simpleHttp (jdconverter ++ "?ID=geophf&jd=900327&format=json") >>= putStrLn
{
"error":false,
"apiversion":"2.0.0",
"data":[
      {
         "jd": 900327.000000,
         "month": 12,
         "day": 17,
         "year": 2249,
         "era": "B.C.",
         "time": "12:00:00.0",
         "tz": 0,
         "dayofweek" : "Tuesday"
      }
   ]
}

... and you can use Data.Aeson to convert these values.
--}

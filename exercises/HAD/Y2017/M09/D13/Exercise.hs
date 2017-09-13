module Y2017.M09.D13.Exercise where

import Data.Time
import Data.Time.Calendar.Julian
import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Y2017.M09.D08.Exercise

{--
There's something funny about the article titles of the Y2017.M09.D08.Exercise

Check out the title names.

Ring any bells? They did to me. These are funny encodings of Julian dates.

Hear me out.

Let's look at:
--}

sampleTitle :: FileName
sampleTitle = "AP900327-0242.txt"

dateFromJulianDate :: Integer -> Day
dateFromJulianDate jd = undefined

-- hint, you may need to do a bit of research here, as it may require more
-- than the functions provided for by Data.Time.Calendar.Julian

-- TODAY'S HASKELL EXERCISE -------------------------------------------------

-- Read in the file names at 

articlesDir :: FilePath
articlesDir = "Y2017/M09/D08/articles/b/"

-- and for each article extract the Julian date (by reading in the first 
-- Integer) and return the corresponding Day published for that article:

fileNameToDay :: FilePath -> IO Day
fileNameToDay dir = undefined

{--
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
myID = undefined -- put your id here, alphanums up to 8 characters

-- and as a courtesy, email the Navy with your id and who you are:

navyEmail :: URL
navyEmail = "navobsy_aa-help@navy.mil"

-- they use this to justify their services

jdconverter :: URL
jdconverter = "http://aa.usno.navy.mil/jdconverter"

-- call the jdconverter with your id and the jd. What do you get?

-- ... you may get a surprise. Just saying.

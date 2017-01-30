module Y2017.M01.D30.Solution where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))

import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Time
import Data.Time.Calendar.Julian

-- below imports available from 1HaskellADay git repository

import Control.Scan.CSV (rend)
import Data.Time.Calendar.Month (mos)

{--
Wheeeeee! We're going to be looking at some orbital (ephemeris) data, but before
we even do that, we have to read in those data, written for FORTRAN and their
ilk, because, you know, satellites.

(that's no reason, but that's as much reason as anyone ever gets when they
are given production data sets and they, in turn, ask "Why is this formatted
this way?")

We'll be working off the JPL Planetary and Lunar Ephemerides data sets:

http://ssd.jpl.nasa.gov/?planet_eph_export

The format of which is described here:

ftp://ssd.jpl.nasa.gov/pub/eph/planets/ascii/ascii_format.txt

(log in as guest or anonymous and it'll let you right in)

I'm looking at the de102 data sets, particularly

header.102                                    at 6 kilobytes
ascm0800.102 (EMbary "Earth-Moon barycenter") at 34 megabytes
ascm1100.102 (Mars)                           at 34 megabytes

Size is an issue, so I'll only look at subsets of the latter files, but today
we won't even look at those latter two files at all, but instead concentrate
on a very tiny subset of the first file.

header.102 contains the following snippet in its preamble:
--}

dateSnippet :: [String]
dateSnippet = ["JPL Planetary Ephemeris DE102/LE051",
               "Start Epoch: JED=  1206160.5-1410-APR-16-00:00:00",
               "Final Epoch: JED=  2817872.5 3002-DEC-22-00:00:00"]

{--
Well, attempt 1: just asking Haskell to do this doesn't work:
*Y2017.M01.D30.Solution> (read "3002-DEC-22") :: Day
*** Exception: Prelude.read: no parse
*Y2017.M01.D30.Solution> (read "3002-DEC-22-00:00:00") :: Day
*** Exception: Prelude.read: no parse

So, let's bear down and do this!

Given the above, convert each line that has a Julian date to a Day.

And, yes, the Start Epoch is in the Julian year NEGATIVE 1410,
And, yes, the Final Epoch is in the Julian year 3002.

el-first-erino: parse out the Julian date stuff from the rest.

Well, one standard, FORTRAN-y way to do that is to count bytes in the string.

I'm not joking.

The other way is to write a parser.

The third way is to call out to the Siri-API.

THERE IS MORE THAN ONE WAY TO SOLVE THIS!

Then there's the el-geophf-erino approach, which is reminiscent of DCGs à la
Prolog (DCG: "Definite Clause Grammar")

Well, a date line has this format:

{Start|Final} Epoch: JED=<right-justified-number><Julian date>-<time>
--}

lineParser :: String -> Maybe Day
lineParser = parseWhich >=> match "Epoch: JED=" >=> parseNum'' >=> parseDay
          -- and ignore the rest of the line

match :: String -> String -> Maybe String
match = stripPrefix

parseWhich, parseNum'' :: String -> Maybe String
parseWhich str = match "Start " str <|> match "Final " str
parseNum'' str = snd <$> parseNum str

parseNum :: String -> Maybe (Float, String)
parseNum = Just . head . reads

{--
*Y2017.M01.D30.Solution> parseNum "  1206160.5-1410-APR-16-00:00:00"
Just (1206160.5,"-1410-APR-16-00:00:00")

*Y2017.M01.D30.Solution> parseNum "  2817872.5 3002-DEC-22-00:00:00"
Just (2817872.5," 3002-DEC-22-00:00:00")
--}

parseDay :: String -> Maybe Day
parseDay = parseNum >=> \(yr, rest) ->
           let (month:day:_) = rend '-' (tail rest)
               m = mos month
               d = read day
           in  return (fromJulian (floor yr) m d)

{--
*Y2017.M01.D30.Solution> parseNum'' "  1206160.5-1410-APR-16-00:00:00" >>= parseDay
Just -1410-04-03

WAAAAAT???

*Y2017.M01.D30.Solution> parseNum'' "  2817872.5 3002-DEC-22-00:00:00" >>= parseDay
Just 3003-01-12
*Y2017.M01.D30.Solution> it >>= return . showJulian ~> Just "3002-12-22"  

AHA! so internal form is Gregorian, and showJulian converts it back to Julian.

Got it!
--}

epoch2Day :: [String] -> [Day]
epoch2Day = mapMaybe lineParser

-- What is the Start Epoch Day value?
-- What is the Final Epoch Day value?

{--
*Y2017.M01.D30.Solution> epoch2Day dateSnippet 
[-1410-04-03,3003-01-12]
--}

-- Throughout this week, we'll be looking at reading in double-precision
-- numbers in scientific notation, and then reading in 3D Cartesian triples
-- And then plotting those triples.
-- And then comparing plots of Earth/Moon barycenters to Mars.

-- Sweet!

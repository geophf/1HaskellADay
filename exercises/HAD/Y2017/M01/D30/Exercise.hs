module Y2017.M01.D30.Exercise where

import Data.Time
import Data.Time.Calendar.Julian

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
Given the above, convert each line that has a Julian date to a Day.

And, yes, the Start Epoch is in the Julian year NEGATIVE 1410,
And, yes, the Final Epoch is in the Julian year 3002.
--}

epoch2Day :: String -> Day
epoch2Day = undefined

-- What is the Start Epoch Day value?
-- What is the Final Epoch Day value?

-- Throughout this week, we'll be looking at reading in double-precision
-- numbers in scientific notation, and then reading in 3D Cartesian triples
-- And then plotting those triples.
-- And then comparing plots of Earth/Moon barycenters to Mars.

-- Sweet!

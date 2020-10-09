module Y2020.M10.D09.Exercise where

{--
Real data are dirty.

This should come as a surprise to noone who has worked with real data. I have.
I was surprised.

Why am I not surprised that I was surprised.

Le sigh.

So, here's the problem I faced. At worked, I was given a set of aggregates-
as-JSON to parse, and my JSON parser puked, right away. "Line 3, Character 19
is illegal."

Line 3, of a 2.5-megabyte JSON file. I opened it up in an editor...

1aggregate.json in this directory

... and I saw NOTHING WRONG.

"THERE! IS! NOTHING! WRONG! YOU! STUPID! JSON! PARSER!" I howled in despair.

So, I wrote out the first three lines, character-for-character...

clean-aggregate.json in this directory

... and it parsed, peachy keen.

Grrrr.
--}

{--
Task 1: If you look at the two files in an ol' ls:

$ ls -l Y2020/M10/D09/*.json
-rw-r--r--@ 1 geophf  staff  63 Oct  9 12:27 Y2020/M10/D09/1aggregate.json
-rw-r--r--  1 geophf  staff  62 Oct  8 13:25 Y2020/M10/D09/clean-aggregate.json

You see there is one single, solitary byte that is removed from clean-aggregate

What. is. that. byte!
--}

import Data.Set (Set)

fileDiff :: FilePath -> FilePath -> IO (Set Int)
fileDiff a b = undefined

{--
Task 2:

ugh. Embedded non-ASCII characters in ASCII-JSON. WHY!

write a cleaner that, given an input supposed-to-be-ASCII-text-file outputs
a for-sure-ASCII-text file.
--}

clean :: FilePath -> FilePath -> IO ()
clean inputFile outputFile = undefined

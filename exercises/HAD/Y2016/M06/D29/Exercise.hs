module Y2016.M06.D29.Exercise where

import Control.Scan.CSV

{--
Okay, today's problem is a tough one! What we have to do is to read in a file
with the following format:

ID|Count|Some other stupid number|Code|Description

And then write out the exact file we read as input, but instead named as:

ABC-VALID-97.txt

or as

XYZ-INVALID-22.txt

based on the Code value.

The Code values are G, B, I, W, D

For the values "G" and "I" write the output as ABC-VALID-97.txt, for the other
values write out the file as "XYZ-INVALID-22.txt"

... now I suppose you can run this in GHCI or some other Haskell interpreter (as
opposed to yesterday's exercise where you had to access the program from the 
shell). But, either way, the input to the program is the input file name, and 
the output of this exercise will be a copy of the file named as per above.

Have at it!
--}

dispatcher :: FilePath -> IO ()
dispatcher = undefined

-- hint: scanning the input file is easy using the above import.

-- With dispatcher defined, apply "goodie.txt"; dispatcher should created
-- an ABC-VALID file. Now apply "baddie.txt" (both in this repository),
-- dispatcher should create an XYZ-INVALID file.

-- goodie.txt URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D29/goodie.txt

-- baddie.txt URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D29/baddie.txt

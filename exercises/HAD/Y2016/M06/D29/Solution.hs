module Y2016.M06.D29.Solution where

import Control.Arrow ((>>>), (&&&))
import Control.Scan.CSV

{--
Format:
ID|Count|Some other stupid number|Code|Description

ABC-VALID-97.txt
XYZ-INVALID-22.txt

based on the Code value.

The Code values are G, B, I, W, D

For the values "G" and "I" write the output as ABC-VALID-97.txt
values write out the file as XYZ-INVALID-22.txt
--}

dispatcher :: FilePath -> IO ()
dispatcher input = readFile input >>=
   uncurry writeFile . (filename . code . decodeMCP &&& id)

-- goodie.txt
-- baddie.txt

data Code = G | B | I | W | D deriving (Show, Read)

data MCP = Master { idx, count, num :: Int, code :: Code, descr :: String }
   deriving Show

decodeMCP :: String -> MCP
decodeMCP = rend '|' >>> \[i,cnt,n,cd,d] ->
   Master (read i) (read cnt) (read n) (read cd) d

filename :: Code -> FilePath
filename G = "ABC-VALID-97.txt"
filename I = "ABC-VALID-97.txt"
filename _ = "XYZ-INVALID-22.txt"   -- I'm feeling very Prolog-y today

{--

So, a program run looks like this:

geophf:HAD geophf$ cd Y2016/M06/D29
geophf:D29 geophf$ ls
Exercise.hs	Solution.hs	baddie.txt	goodie.txt

(no output files)

geophf:D29 geophf$ ghci Solution.hs 
*Y2016.M06.D29.Solution> dispatcher "goodie.txt"

geophf:D29 geophf$ ls
ABC-VALID-97.txt	Solution.hs		goodie.txt
Exercise.hs		baddie.txt

There's our valid output

Okay, let's remove that output and run again with baddie.txt:

geophf:D29 geophf$ rm ABC-VALID-97.txt 
geophf:D29 geophf$ ghci Solution.hs 
*Y2016.M06.D29.Solution> dispatcher "baddie.txt" 
geophf:D29 geophf$ ls
Exercise.hs		XYZ-INVALID-22.txt	goodie.txt
Solution.hs		baddie.txt

And there's our invalid output file.

Two weeks in javer to develop this solution on a Big Gov't project, smh.
--}

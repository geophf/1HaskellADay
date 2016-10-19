module Y2016.M07.D01.Solution where

import Control.Monad ((>=>))
import Data.List (intercalate)
import Text.HTML.TagSoup

{--
So, jUnit on eclipse has an option to save out the unit test results.

Nice!

As XML.

Ummmm, ... nice?

Well, the manager wants a spreadsheet of all test results, see? They want a
spreadsheet ... because they are managers.

So, the saved XML is at test.xml at this directory, which is also at the URL: 

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M07/D01/test.xml

YOU want to save out a CSV-file in the following format

name,class,time

Your mission, should you decide to accept it, is to read in the test.xml file
then to write out the unit tests as CSV rows as test.csv
--}

junitXML2CSV :: FilePath -> IO ()
junitXML2CSV =
   readFile >=> mapM_ (putStrLn . intercalate ",")
              . (columnHeaders:)
              . map extractAttribs
              . filter (isTagOpenName "testcase") . parseTags

extractAttribs :: Tag String -> [String]
extractAttribs tag = [fromAttrib] <*> attribs <*> [tag]

attribs, columnHeaders :: [String]
attribs = words "name classname time"
columnHeaders = ["Test Name","Class Tested","Test Execution Time"]

{--
*Y2016.M07.D01.Exercise> junitXML2CSV "Y2016/M07/D01/test.xml" 
name,classname,time
HIMX,AYA.BCPC.ASNA.MPC.BLOX.ACT-A,0.0
GRP.UN,AYA.BCPC.ASNA.MPC.BLOX.ACT-A,0.0
JBL,AYA.BCPC.ASNA.MPC.BLOX.ACT-A,0.0
KING,AYA.BCPC.ASNA.MPC.BLOX.ACT-A,0.0
CFG,AYA.BCPC.ASNA.MPC.BLOX.AKRX,0.0
C,AYA.BCPC.ASNA.MPC.BLOX.AKRX,0.0
LVS,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
IMO,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
HTZ,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
MFG,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
CLDX,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
COKE,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
CSCO,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
CXRX,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
DEPO,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
EEM,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
EPE,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
F,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
FIVE,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
GBT,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
GLNG,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0
LINE,AYA.BCPC.ASNA.MPC.BLOX.AMOV,0.0

And there you have it! YAY!
--}

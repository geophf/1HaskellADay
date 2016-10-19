module Y2016.M07.D04.Solution where

import Control.Arrow ((&&&), (>>>))
import Control.Monad ((>=>))
import Text.HTML.TagSoup

{--
The name of the game for today's Haskell exercise is report generation.

You have the jUnit test case run-off as XML at Y2016/M07/D01/

You want to generate a report of this scheme:
--}

genReport :: TestRun -> [String]
genReport (ROFF proj t s f e i) =
   let p = s - f - e - i in
   ["-- REPORT " ++ replicate 65 '-', "",
    "Test Case Overview:," ++ proj, "",
    "Total Tests:," ++ show t,
    "Total Started:," ++ show s,
    "Total Passed:," ++ show p,
    "Total Failures:," ++ show f,
    "Total Errors:," ++ show e,
    "Total Ignored:," ++ show i, "",
    if s == p then "All unit test cases passed."
    else "WAIT! WHAT? YOU'RE ALL FIRED!", "",
    "-- END-REPORT " ++ replicate 61 '-', ""]

reportTestSummary :: FilePath -> IO ()
reportTestSummary = readFile >=> mapM_ putStrLn . genReport . runoff . parseTags

data TestRun =
   ROFF { project :: String, tests, started, failures, errors, ignored :: Int }
      deriving Show

runoff :: [Tag String] -> TestRun
runoff =
   (head &&& map read . tail >>> uncurry reify)
   . getTheThings
   . filter (isTagOpenName "testrun")

reify :: String -> [Int] -> TestRun
reify proj [t,s,f,e,i] = ROFF proj t s f e i

getTheThings :: [Tag String] -> [String]
getTheThings tag = [fromAttrib] <*> attribs <*> tag

attribs :: [String]
attribs = words "project tests started failures errors ignored"

{--
*Y2016.M07.D04.Solution> reportTestSummary "Y2016/M07/D01/test.xml" 
-- REPORT -----------------------------------------------------------------

Test Case Overview:     AA

Total Tests:            22
Total Started:          22
Total Passed:           22
Total Failures:         0
Total Errors:           0
Total Ignored:          0

All unit test cases passed.

-- END-REPORT -------------------------------------------------------------

I didn't get fired! YAY!
--}

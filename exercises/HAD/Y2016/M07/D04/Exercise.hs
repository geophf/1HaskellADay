module Y2016.M07.D04.Exercise where

import Text.HTML.TagSoup

{--
The name of the game for today's Haskell exercise is report generation.

You have the jUnit test case run-off as XML at Y2016/M07/D01/ and here:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M07/D01/test.xml

You want to generate a report of this scheme:

-- REPORT ----------------------------------------------------------------

Test Case Overview:     $testrun/@project

Total Tests:            $testrun/@tests
Total Started:          $testrun/@started
Total Passed:           $testrun/@started - @failures - @errors - @ignored
Total Failures:         $testrun/@failures
Total Errors:           $testrun/@errors
Total Ignored:          $testrun/@ignored

All unit test cases passed.

-- END-REPORT ------------------------------------------------------------

(that is, IF all the unit test cases passed. If they don't then, of course,
fire the developers, because it's all their fault.)

(I joke)

(Unless I'm a manager)

Write a haskell function that from the jUnit XML generates the above report.
--}

reportTestSummary :: FilePath -> IO ()
reportTestSummary = undefined

-- Hint: parsing the XML may be easy(er?) with TagSoup or another XML parser

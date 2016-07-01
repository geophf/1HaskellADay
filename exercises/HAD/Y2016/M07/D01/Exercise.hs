module Y2016.M07.D01.Exercise where

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
junitXML2CSV = undefined

-- hint: You can use tagsoup, or you can use any XML parsing library you so
-- choose

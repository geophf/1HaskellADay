{-# LANGUAGE ViewPatterns #-}

module Data.Time.Calendar.Month where

import Control.Arrow ((&&&))
import Data.Array
import Data.Char (toUpper)
import Data.Time.Calendar

{-- a solution to the problem posted at http://lpaste.net/1501657254015795200
@1HaskellADay solution for 2016-02-02

So, we have a problem.

We have dates in this format:

January 01, 2016
January 18, 2016
February 15, 2016
March 25, 2016
May 30, 2016
July 04, 2016
September 05, 2016
November 24, 2016
November 25, 2016
December 26, 2016

But we want them to be, you know: Day-values.

Today's problem. Convert the above values into Day-values.
--}

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
   deriving (Eq, Ord, Enum, Read, Show)

mosDayYrToDay :: String -> Day
mosDayYrToDay (words -> [mos, dayComma, yr]) =
   let tehmonth = read mos :: Month in
   fromGregorian (read yr) (succ $ fromEnum tehmonth) (read $ init dayComma)

-- to do the above function, you may need to 'read' in a Month value ... maybe.

-- convert dates to [Day]:

dates :: [String]
dates = ["January 01, 2016","January 18, 2016","February 15, 2016",
         "March 25, 2016","May 30, 2016","July 04, 2016","September 05, 2016",
         "November 24, 2016","November 25, 2016","December 26, 2016"]

{--
*Data.Time.Calendar.Month> map mosDayYrToDay dates ~>
        [2016-01-01,        2016-01-18,        2016-02-15,
         2016-03-25,      2016-05-30,     2016-07-04,     2016-09-05,
         2016-11-24,          2016-11-25,         2016-12-26]
--}

{-- BONUS -------------------------------------------------------------------

Show each input string alongside each resulting Day
--}

datesandDays :: [String] -> [(String, Day)]
datesandDays = map (id &&& mosDayYrToDay)

{--
*Data.Time.Calendar.Month Control.Arrow> mapM_ print $ datesandDays dates
("January 01, 2016",2016-01-01)
("January 18, 2016",2016-01-18)
("February 15, 2016",2016-02-15)
("March 25, 2016",2016-03-25)
("May 30, 2016",2016-05-30)
("July 04, 2016",2016-07-04)
("September 05, 2016",2016-09-05)
("November 24, 2016",2016-11-24)
("November 25, 2016",2016-11-25)
("December 26, 2016",2016-12-26)
--}

-- Now we need abbreviated months for tweets:

data AbbrevMonth = JAN | FEB | MAR | APR | MAY | JUN | JUL
                 | AUG | SEP | OCT | NOV | DEC
   deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)

readTweetDate :: String -> Day
readTweetDate (words -> [_,month,day,_,_,yr]) =
   fromGregorian (read yr) (mos month) (read day)

mos :: String -> Int
mos = succ . fromEnum . readMos

readMos :: String -> AbbrevMonth
readMos = read . map toUpper

{--
An example from the JSON

*Data.Time.Calendar.Month> readTweetDate "Tue Apr 26 01:30:36 +0000 2016" ~>
2016-04-26

... which also happens to be my birthday, oddly enough ... SWEET!
--}

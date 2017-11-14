module Y2017.M11.D14.Solution where

{--
I've been using the Control.Scan.CSV module for quite some time and have been
quite happy with it, but then, along comes the real world and throws a wrench
into all this happiness.
--}

import Control.Arrow ((***), (>>>), second)

-- below import available via 1HaskellADay git repository

import Control.Scan.CSV hiding (escapedThenCSVd)

{--
As you see from the tests in that module, CSV parses, well, comma-separated
values, obviously, but it also treats quoted strings as one column, so we have:

>>> csv "1,2,3"
["1","2","3"]

3 columns

>>> csv "\"1,2\",3"
["1,2","3"]

2 columns

>>> csv "\"1,2\",3,4,5,\"6,7,8\",9,10"
["1,2","3","4","5","6,7,8","9","10"]

7 columns

>>> csv "\"1,2\",3,4,5,\"6,7,8\",\"9,10\""
["1,2","3","4","5","6,7,8","9,10"]

6 columns

And that's great. Working as desired.

But now we come to this:
--}

quotedLine' :: String
quotedLine' = "11,Education Department Withdraws Controversial ESSA Spending "
          ++ "Proposal,1/18/17,Alyson Klein,\"That big fight over spending "
          ++ "rules for the Every Student Succeeds Act has ended not with a "
          ++ "bang, but a whimper: U.S. Secretary of Education John B. King, "
          ++ "Jr. is throwing in the towel, withdrawing a proposed regulation "
          ++ "for a section of the law known as \"\"supplement-not-supplant"
          ++ "\"\" that had strong backing in the civil rights community, but "
          ++ "angered state chiefs, advocates for districts, and Republicans "
          ++ "in Congress.\",http://blogs.edweek.org/edweek/campaign-k-12/ass"
          ++ "ets_c/2016/12/King%20blog%20pic-thumb-500x331-22556.jpg,"
          ++ "http://blogs.edweek.org/edweek/campaign-k-12/2017/01/essa_john_"
          ++ "b_king_jr_withdraws_.html,\"2,000,000\",11"

-- You see what's going on here? Quoted strings within quoted columns are
-- doubly double-quoted. We need a CSV that does all the above AND recognizes
-- doubly double-quotes as part of the column, not as a column separator.

-- So, today's Haskell problem is to write a CSV processor that is savvy to
-- embedded quoted strings within a column.

csv' :: String -> [String]
csv' "" = []
csv' str = (rend ',' *** escapedThenCSVd >>> uncurry (++)) (break (== '"') str)

-- okay, that's all the same as before, but now we need a new way of escaping
-- columns that take into account embedded quotes

{--
Write csv' so that all the csv parsing as above still work AND the above
quotedLine is properly parsed into a 9-column list of strings.
--}

escapedThenCSVd :: String -> [String]
escapedThenCSVd "" = []
escapedThenCSVd str =
   let (reg, quoted) = uncurry check2DoubleQuote (break (== '"') (tail str))

-- so here's the problem, if this is not an end-quote, then we need to continue
-- parsing reg ++ quoted as one contiguous string.

-- but also here's the problem. If it is the end-quote AND it's the last column
-- then we're running up against _|_ for lists.

   in (second (csv . drop 2) >>> uncurry (:)) (reg, quoted)

-- so the magic is our doubly double-quoted string checker.

check2DoubleQuote :: String -> String -> (String, String)
check2DoubleQuote pre quoted =
   if take 2 quoted == "\"\""
   then let (post, newquoted) = break (== '"') (drop 2 quoted) in
        check2DoubleQuote (pre ++ ('"':post)) newquoted
   else (pre, quoted)

{--
>>> mapM_ putStrLn (csv' quotedLine')
11
Education Department Withdraws Controversial ESSA Spending Proposal
1/18/17
Alyson Klein
That big fight over spending rules for the Every Student Succeeds Act has ended 
not with a bang, but a whimper: U.S. Secretary of Education John B. King, Jr. 
is throwing in the towel, withdrawing a proposed regulation for a section of 
the law known as "supplement-not-supplant" that had strong backing in the civil 
rights community, but angered state chiefs, advocates for districts, and 
Republicans in Congress.
http://blogs.edweek.org/edweek/campaign-k-12/assets_c/2016/12/King%20blog%20pic-thumb-500x331-22556.jpg
http://blogs.edweek.org/edweek/campaign-k-12/2017/01/essa_john_b_king_jr_withdraws_.html
2,000,000
11

There you go: quoted strings within columns of CSV. Moving this to the CSV
module.
--}

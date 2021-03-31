module Control.Scan.CSV where

{--
A set of functions around tokenizing/tokenising/rending lists and then patching
them back up together again/weaving/intercalating <-- smh: 'intercalate'? really?
--}

import Control.Arrow
import Data.List (intercalate)

-- below imports available via 1HaskellADay git repository

import Control.List (weave,softtail)
import Control.Presentation

-- I do this often enough: scanning in a CSV file, so, here it is:

-- your words function with a 'by'-argument. Should be there already ...

-- note something interesting about wordsBy: nowhere is this list necessarily
-- a list of characters. We can generalize this.

wordsBy :: Eq a => [a] -> [a] -> [[a]]
wordsBy [char] = rend char
wordsBy delims = rendBy (`elem` delims)

{-- e.g.:

*Main> wordsBy "|" "REQMT209||" ~> ["REQMT209"]

*Control.Scan.CSV> let line = "REQMT207|REQMT228, REQMT57|REQMT228, REQMT57"
*Control.Scan.CSV> wordsBy "|" line ~>
["REQMT207","REQMT228, REQMT57","REQMT228, REQMT57"]

*Control.Scan.CSV> wordsBy "|," line ~>
["REQMT207","REQMT228"," REQMT57","REQMT228"," REQMT57"]

Note the leading spaces, so it should be:

*Control.Scan.CSV> wordsBy "|, " line ~>
["REQMT207","REQMT228","REQMT57","REQMT228","REQMT57"]

An alternate implementation that replaces the original hand-coded wordsBy:
--}

rend :: Eq a => a -> [a] -> [[a]] -- practice shows 1 delimiter only
rend delim = rendBy (== delim)

rendBy :: (a -> Bool) -> [a] -> [[a]]
rendBy fn [] = []
rendBy fn line@(h:t) =
    if fn h then ([]:rendBy fn t)  -- to keep columns aligned
    else (second (rendBy fn . softtail) >>> uncurry (:)) $ break fn line

-- *Main> rend ',' "1,2,3" ~> ["1","2","3"]

-- csv = rend ',' -- so we could just write this.

-- No, we need to make csv "-escape-aware

csv :: String -> [String]
csv "" = []
csv str = (rend ',' *** escapedThenCSVd >>> uncurry (++)) (break (== '"') str)

escapedThenCSVd :: String -> [String]
escapedThenCSVd "" = []
escapedThenCSVd str =
   let (reg, quoted) = uncurry checkDoublyDoubleQuote (break (== '"') (tail str))

-- so here's the problem, if this is not an end-quote, then we need to continue
-- parsing reg ++ quoted as one contiguous string.

-- but also here's the problem. If it is the end-quote AND it's the last column
-- then we're running up against _|_ for lists.

   in (second (csv . drop 2) >>> uncurry (:)) (reg, quoted)

checkDoublyDoubleQuote :: String -> String -> (String, String)
checkDoublyDoubleQuote pre quoted =
   if take 2 quoted == "\"\""
   then let (post, newquoted) = break (== '"') (drop 2 quoted) in
        checkDoublyDoubleQuote (pre ++ ('"':post)) newquoted
   else (pre, quoted)

{--
>>> csv "1,2,3" 
["1","2","3"]
>>> csv "\"1,2\",3" 
["1,2","3"]
>>> csv "\"1,2\",3,4,5,\"6,7,8\",9,10" 
["1,2","3","4","5","6,7,8","9","10"]
>>> csv "\"1,2\",3,4,5,\"6,7,8\",\"9,10\"" 
["1,2","3","4","5","6,7,8","9,10"]

There ya go! Quote escaping csv-parser. Yay.

Not quite.

We need to scan this line, too:
--}

quotedLine :: String
quotedLine = "11,Education Department Withdraws Controversial ESSA Spending "
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

-- Straight from Prelude.unwords using 'sep' instead of spaces as separators.
-- isn't this intercalate? In which case (intercalate is just a weird word for
-- a function name, isn't it), then, aren't we weaving the separator into this
-- list of sublists?

unwordsBy :: Char -> [String] -> String
unwordsBy char = intercalate (pure char)

uncsv :: Univ a => a -> String
uncsv = weave . explode

-- What happens when a string has an embedded comma? We quote it.

-- ... um, ... on second thought: no.

enquotify :: String -> String
enquotify str | any (== ',') str = '"':str ++ pure '"'
              | otherwise        = str

{--
let ans = ["REQMT207","REQMT228","REQMT57","REQMT228","REQMT57"]
*Control.Scan.CSV> unwordsBy ',' ans ~>
"REQMT207,REQMT228,REQMT57,REQMT228,REQMT57"

So this was a little exercise: How to convert this line:

REQMT207|REQMT228, REQMT57|REQMT228, REQMT57

Into 'just' this csv line:

REQMT207,REQMT228,REQMT57,REQMT228,REQMT57
--}

-- and if we have to read something into the Maybe-domain:

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

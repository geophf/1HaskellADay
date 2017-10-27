module Control.Scan.CSV where

{--
A set of functions around tokenizing/tokenising/rending lists and then patching
them back up together again/weaving/intercalating <-- smh: 'intercalate'? really?
--}

import Control.Arrow
import Data.List (intercalate)

-- below imports available via 1HaskellADay git repository

import Control.List (weave)
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

softtail :: [a] -> [a]
softtail [] = []
softtail (h:t) = t

-- *Main> rend ',' "1,2,3" ~> ["1","2","3"]

csv :: String -> [String]
-- csv = rend ',' -- so we could just write this.

-- No, we need to make csv "-escape-aware

csv "" = []
csv str = (rend ',' *** escapedThenCSVd >>> uncurry (++)) (break (== '"') str)

escapedThenCSVd "" = []
escapedThenCSVd str =
   (second (csv . softtail . softtail) >>> uncurry (:)) (break (== '"') (tail str))

{--
*Main> csv "1,2,3" ~> ["1","2","3"]
*Main> csv "\"1,2\",3" ~> ["1,2","3"]
*Main> csv "\"1,2\",3,4,5,\"6,7,8\",9,10" ~> ["1,2","3","4","5","6,7,8","9","10"]
*Main> csv "\"1,2\",3,4,5,\"6,7,8\",\"9,10\"" ~> ["1,2","3","4","5","6,7,8","9,10"]

There ya go! Quote escaping csv-parser. Yay.
--}

-- Straight from Prelude.unwords using 'sep' instead of spaces as separators.
-- isn't this intercalate? In which case (intercalate is just a weird word for
-- a function name, isn't it), then, aren't we weaving the separator into this
-- list of sublists?

unwordsBy :: Char -> [String] -> String
unwordsBy char = intercalate (pure char)

uncsv :: Univ a => a -> String
uncsv = weave . map enquotify . explode

-- What happens when a string has an embedded comma? We quote it.

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

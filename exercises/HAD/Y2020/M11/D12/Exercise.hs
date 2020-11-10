{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D12.Exercise where

-- Well, howdy! Yesterday, ...

-- (i.e.: import Y2020.M11.D11.Exercise)

{--
... we were all: "We're gonna upload alliances to the graph-store!"

But we didn't because the day ended in an unicode-error. *sad-face*

So, today, let's:

1. first, identify the values that have unicode-points outside ASCII
2. then, replace those values with plain-vanilla ASCII values
3. finally, upload the newly updated AllianceMap to the graph
   (which we tried yesterday, so this step should be easy).
--}

import Y2020.M10.D30.Exercise             -- for AllianceMap

import Data.Text

-- for Step 1, look at, e.g. Y2020.M10.D23.Exercise.stripNonAscii

isNonAscii :: Text -> Bool
isNonAscii = undefined

-- How many thingies (that's a technical term) are non-ascii?

-- for Step 2, we can do this generically or we can do it specifically.

cleanText :: Text -> Text
cleanText = undefined

-- `cleanText` given text isNonAscii replace it with Text value that is ASCII

-- So, then, we cleanify (that's a word, now) the AllianceMap thusly:

cleanify :: AllianceMap -> AllianceMap
cleanify = undefined

-- now upload the alliances to the graph (as yesterday).

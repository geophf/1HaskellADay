{-# LANGUAGE OverloadedStrings #-}

module Y2018.M05.D18.Exercise where

-- Before we insert new articles, let's step back in time for a moment. Recall

import Y2018.M04.D09.Exercise

{--
where we uploaded tags, we uploaded a sample set of 10 tags. There are a lot
more tags than 10 for the World Policy Journal. Today we're going to find out
how many.

The REST endpoint for the tags is as follows:
--}

import Y2018.M04.D11.Exercise (PageNumber)

tagEndpoint :: PageNumber -> FilePath
tagEndpoint pn = "https://worldpolicy.org/wp-json/wp/v2/tags?per_page=100&page="
   ++ show pn

-- The JSON format we already know and parse (see above exercise) and the
-- tags are exhausted when the Value returned is the empty list.

-- How many tags are there for this REST endpoint?

downloadTags :: IO [Tag]
downloadTags = undefined

-- You may want to use some pagination function to help define downloadTags

-- hint: see definitions in Y2018.M04.D13 for downloading packets and use
-- a similar approach

{-- BONUS -----------------------------------------------------------------

Create an application that downloads all the tags from the REST endpoint and
then uploads those tags to a PostgreSQL data table as described in the module
Y2018.M04.D09.

-- BONUS-BONUS ------------------------------------------------------------

Create an application that does the same thing, but for categories this time.

See Y2018.M04.D06 for information on categories (very (VERY)) much like tags.
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D20.Exercise where

{--
So, YESTERDAY's Haskell problem...

Well, we didn't have a Haskell problem yesterday, because the problem I did
write: "Upload the World Policy Journal archived from the REST endpoint to a
SQL RDS you created," turned out to be a bigger problem than I anticipated, 
even after the groundwork I laid out in previous exercises.

So, let's break this elephant-sized problem into bite-sized pieces, shall we?

Problem one, when I uploaded the archive to the RDS and inspected the results,
is this:

"title": {
                "rendered": "Russia&#8217;s Alternative Political Life"
            },

Yeah, you saw that right: there are HTML entities encoded in titles of these
articles. And I'm like: REALLY? And, yes, really.

So, given a set of titles, return their plain-text equivalents. I won't tell
you how to do this, do your own research. Maybe look at HTML libraries in
Haskell.
--}

deHTMLize :: String -> String
deHTMLize title = undefined

-- deHTMLize the following titles:

titles :: [String]
titles = [ "Trajectory of the U.S.-China Trade Impasse",
           "Russia&#8217;s Alternative Political Life",
           "Africa&#8217;s Defense Against Non-Communicable Disease",
           "In Print: &#8220;Dissident Cinema&#8221;",
           "In Print: &#8220;Only a Shadow&#8221;",
           "In Print: &#8220;Life on Mars&#8221;",
           "The Contentious U.S. Presence in Okinawa, Japan",
           "Displaced in Darfur"]

module Y2017.M03.D13.Exercise where

import Network.HTTP.Conduit

{--

Oh, yeah. Let's do this one!

https://twitter.com/HaggardHawks/status/840321681726013440

Nic Wilkinson Retweeted
Haggard Hawks @HaggardHawks  Mar 10
All fourteen lines in David Shulman’s sonnet ‘Washington Crossing The Delaware’ 
(1936) are anagrams of the title.

prove it!
--}

anagram :: String -> String -> Bool
anagram line = undefined

-- snaps for defining anagram elegantly

sonnet :: FilePath
sonnet = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2017/M03/D13/sonnet.txt"

{--
So, read in the first line. Use it as the basis of the anagram. 

For each line thereafter:

Read in each line. Remove non-alpha characters. Determine if that line is an
anagram of the first line.

What are your results?
--}

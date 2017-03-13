module Y2017.M03.D13.Solution where

import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (toUpper, isAlpha)
import Data.Function (on)
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Data.Bag (Bag)
import qualified Data.Bag as Bag

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
-- anagram source = (== Bag.fromList source) . Bag.fromList
anagram = (==) `on` Bag.fromList  -- via Denis Stoyanov @xgrommx

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

anagramify :: FilePath -> IO Bool
anagramify = fmap (and . map snd) . (simpleHttp >=> isAnagramific . unpack)

isAnagramific :: String -> IO [(String, Bool)]
isAnagramific poem =
   let (title:_author:ltrs) = map (map toUpper . filter isAlpha) (lines poem)
       ana = anagram title
   in  mapM (\line -> let ans = (line, ana line) in print ans >> return ans)
            (filter (not . null) ltrs)

{--
>>> anagramify sonnet 
("AHARDHOWLINGTOSSINGWATERSCENE",True)
("STRONGTIDEWASWASHINGHEROCLEAN",True)
("HOWCOLDWEATHERSTINGSASINANGER",True)
("OSILENTNIGHTSHOWSWARACEDANGER",True)
("THECOLDWATERSSWASHINGONINRAGE",True)
("REDCOATSWARNSLOWHISHINTENGAGE",True)
("WHENSTARGENERALSACTIONWISHDGO",True)
("HESAWHISRAGGEDCONTINENTALSROW",True)
("AHHESTANDSSAILORCREWWENTGOING",True)
("ANDSOTHISGENERALWATCHESROWING",True)
("HEHASTENSWINTERAGAINGROWSCOLD",True)
("AWETCREWGAINHESSIANSTRONGHOLD",True)
("GEORGECANTLOSEWARWITHSHANDSIN",True)
("HESASTERNSOGOALIGHTCREWANDWIN",True)
True

Q.E.D.

translated from the Latin: There you have it, then!
--}

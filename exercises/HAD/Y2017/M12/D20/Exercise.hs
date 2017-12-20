{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D20.Exercise where

{--
Today's Haskell problem is to set up tomorrow's Haskell problem.

Fancy.

So, we have a large-ish JSON file with some header information and then a list
of maps, the maps are large and there's 100 entries (or so the header says).

Well, we only want to look at 5 entries, and we DON'T want to look at the 
first entry, because the first entry is 'yucky' by some measure of 'yuckiness.'

So, today's Haskell problem: read in the JSON and spit out the same-ish JSON
but a smaller set.

To wit:

read in the header, spit out* the header
read in the list of 100 elements, spit out* 5 elements, excluding the first one

*'spit out' is a technical term.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString)

data Packet =
      Pack { view :: String, count, total, next :: Int, prev :: Maybe Int, rows :: [Article] }
      deriving (Eq, Show)

instance ToJSON Packet where
   toJSON pack = undefined

instance FromJSON Packet where
   parseJSON obj = undefined

type Article = Value

-- for now, our article values are just value-placeholders.

sampleNoRows :: ByteString
sampleNoRows = "{\"view\":\"default\",\"count\":100,\"total\":279091,\"next\":100,\"prev\":null,\"rows\":[]}"

{--
>>> (decode sampleNoRows) :: Maybe Packet
Just (Pack {view = "default", count = 100, total = 279091, next = 100, prev = Nothing})

>>> Just packy = it

>>> encode packy
"{\"next\":100,\"count\":100,\"total\":279091,\"view\":\"default\",\"prev\":null,\"rows\":[]}"
--}

dir :: FilePath
dir = "Y2017/M12/D20/"

readSample :: FilePath -> IO Packet
readSample file = undefined

-- writes out JSON of n articles 

writeSubset :: FilePath -> Int -> Packet -> IO ()
writeSubset out n pack = undefined

{--
>>> pac <- readSample (dir ++ "sample.json")
>>> writeSubset (dir ++ "subset.json") 5 pac

$ ls -l Y2017/M12/D20/*.json
-rw-r--r--@ 1 geophf  staff  589770 Dec 13 12:53 Y2017/M12/D20/sample.json
-rw-r--r--@ 1 geophf  staff   26946 Dec 19 20:02 Y2017/M12/D20/subset.json

WOOT!
--}

-- Cool! Now we're ready for tomorrow's exercise!

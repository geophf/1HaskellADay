{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D20.Solution where

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
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import qualified Data.Text as T

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))

data Packet =
   Pack { view :: String, count, total, next :: Int, prev :: Maybe Int,
          rows :: [UnparsedArticle] }
      deriving (Eq, Show)

instance ToJSON Packet where
   toJSON packy = object (["view" .= view packy, "prev" .= prev packy,
                           "rows" .= rows packy]
         ++ zipWith (.=) (T.words "count total next")
                         ([count, total, next] <*> [packy]))

instance FromJSON Packet where
   parseJSON (Object o) =
      Pack <$> o .: "view" <*> o .: "count" <*> o .: "total"
           <*> o .: "next" <*> o .: "prev"  <*> o .: "rows"

sampleNoRows :: ByteString
sampleNoRows = "{\"view\":\"default\",\"count\":100,\"total\":279091,\"next\":100,\"prev\":null,\"rows\":[]}"

{--
>>> (decode samplePacket) :: Maybe Packet
Just (Pack {view = "default", count = 100, total = 279091, next = 100, prev = Nothing})

>>> Just packy = it

>>> encode packy 
"{\"next\":100,\"count\":100,\"total\":279091,\"view\":\"default\",\"prev\":null,\"rows\":[]}"
--}

type UnparsedArticle = Value

dir :: FilePath
dir = "Y2017/M12/D20/"

readSample :: FilePath -> IO Packet
readSample file = fromJust . decode <$> BL.readFile file

-- writes out JSON of n articles 

writeSubset :: FilePath -> Int -> Packet -> IO ()
writeSubset out = BL.writeFile out . encodePretty <<- nRowsOf

nRowsOf :: Int -> Packet -> Packet
nRowsOf n p = p { rows = take n (tail (rows p)) }

{--
>>> pac <- readSample (dir ++ "sample.json")
>>> writeSubset (dir ++ "subset.json") 5 pac

$ ls -l Y2017/M12/D20/*.json
-rw-r--r--@ 1 geophf  staff  589770 Dec 13 12:53 Y2017/M12/D20/sample.json
-rw-r--r--@ 1 geophf  staff   26946 Dec 19 20:02 Y2017/M12/D20/subset.json

WOOT!
--}

-- Cool! Now we're ready for tomorrow's exercise!

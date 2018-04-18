{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D17.Solution where

{--
So, yesterday we had a bunch of JSON from various periodicals, but they were
all in the same JSON format, which made it simple to upload them into a common
database.

Today, we have JSON, but in a different format, that we need to upload into
that database, so, let's do it! YES!

Also, the size is different, so it will be compressed this time, FER SHUR!
--}

import qualified Codec.Compression.GZip as GZ

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (ord)
import Data.Maybe (fromJust)

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection

import Y2018.M04.D16.Solution

data ViceArticles = VA { arts :: [Vice] }

data Vice = Vice { vid :: Integer, vart :: String }
   deriving Show

-- of course, Vice has a hashed id, but oh, well. Use its idx, instead

-- The articles are in this directory:

viceDir :: FilePath
viceDir = "Y2018/M04/D17/compressed/"

-- and the files are:

vices :: [FilePath]
vices = map (("vice-" ++) . (++ ".json")) (words "rands tops")

-- remember to add ".gz" but I did that for ya yesterday.

instance FromJSON Vice where
   parseJSON (Object o) = Vice <$> o .: "idx" <*> o .: "body"

instance FromJSON ViceArticles where
   parseJSON (Object o) = VA <$> o .: "arts"

vice2Art :: Vice -> IxArt
vice2Art (Vice i b) = IA (show i) (map replaceNonASCII b)

replaceNonASCII :: Char -> Char
replaceNonASCII c = if ord c > 127 then ' ' else c

-- parse in the vice articles then save them to the articles database.

-- Of course, first, you need to add "VIC" as a publisher and the vices
-- as file types to the database. See yesterday's exercise and do that.

readVices :: FilePath -> IO [IxArt]
readVices =
   fmap (map vice2Art . arts . fromJust . decode . GZ.decompress)
      . BL.readFile . (++ ".gz")

{--
>>> json <- readVices (viceDir ++ "rands.json")
>>> length json
30
>>> (ix &&& take 50 . text) $ head json
("869"," It was hate on first sight. This dude was sitting")
---}

-- with the above readVices function, you should be able to call main'' and go!

{--
So now we do the set-up and delivery.

>>> withConnection ARCHIVE (\c -> insertSourceFiles c (map Src vices) >>
                                  void (insertPubs c [Pub "VIC"]))

... and then ...

>>> main'' readVices (viceDir:vices)
etlArticleSet start...
... done.

TA-DAH!
--}

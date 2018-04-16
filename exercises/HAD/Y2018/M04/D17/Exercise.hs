{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D17.Exercise where

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

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)

import Y2018.M04.D16.Exercise

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

instance FromJSON ViceArticles where
   parseJSON (Object o) = undefined

instance FromJSON Vice where
   parseJSON (Object o) = undefined

vice2Art :: Vice -> IxArt
vice2Art v = undefined

-- parse in the vice articles then save them to the articles database.
-- Also, make sure non-ASCII characters are removed from the text, because ick.

-- Of course, first, you need to add "VIC" as a publisher and the vices
-- as file types to the database. See yesterday's exercise and do that.

-- hint: use withConnection ARCHIVE to get a connection to the ARCHIVE database

readVices :: FilePath -> IO [IxArt]
readVices vicedir = undefined -- remember to append ".gz"

-- with the above readVices function, you should be able to call main'' and go!

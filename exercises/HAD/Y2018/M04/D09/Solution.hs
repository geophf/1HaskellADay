{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D09.Solution where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

{--
Okay, fam. We're going to wrap up our preparation for the data load by
parsing tags into a tags lookup table.
--}

-- below imports available via 1HaskellADay git repository

import Data.LookupTable

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import Y2018.M04.D06.Solution

tags :: FilePath
tags = "Y2018/M04/D09/tags.json"

{-- The tags are stored on the REST endpoint here:

https://worldpolicy.org/wp-json/wp/v2/tags?per_page=100
--}

-- welp, since the tags.json has the same structure (that we care about) as
-- the cats.json, we just copy-over the type

type Tag = Category

readTags :: FilePath -> IO [Tag]
readTags = readCats

{--
>>> readTags tags >>= mapM_ (\(IxV i (Cat c)) -> print (i,c))
(3904,"12x12")
(4095,"18th National Congress")
(4133,"18th Party Congress")
(7677,"1917")
(3302,"1953 Armistice")
(2707,"1967 Line")
(702,"1968")
(234,"2001")
(232,"2002")
(254,"2003")
--}

-- um, okay. That was easier than expected.

tagStmt :: Query
tagStmt = [sql|INSERT INTO tag VALUES (?,?)|]

insertTags :: Connection -> LookupTable -> IO ()
insertTags = insertion tagStmt

{--
>>> withConnection WPJ (flip insertTags (cats2lk json))

then:

$ select * from tag;

id	tag
3904	12x12
4095	18th National Congress
4133	18th Party Congress
7677	1917
3302	1953 Armistice
2707	1967 Line
702	1968
234	2001
232	2002
254	2003

TA-DAH!
--}

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D09.Exercise where

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

tags :: FilePath
tags = "Y2018/M04/D09/tags.json"

data Tag = SomeStructureYouDeclare

readTags :: FilePath -> IO [Tag]
readTags json = undefined

-- and then, into the database we go:


tagStmt :: Query
tagStmt = [sql|INSERT INTO tag VALUES (?,?)|]

insertTags :: Connection -> LookupTable -> IO ()
insertTags conn = undefined

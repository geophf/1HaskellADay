{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M05.D25.Exercise where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Network.HTTP.Conduit
import Network.HTTP.Types hiding (Query)

-- below imports available via 1HaskellADay

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging

{--
Okay, let's do a little testy-testy.

Download some articles from the database, upload them to a rest endpoint, and
eh, that's today's exercise.
--}

nerEndpoint :: FilePath
nerEndpoint = "https://grby98heb8.execute-api.us-east-1.amazonaws.com/CORS"

{-- 

The curl command is:

curl -X POST -H "Content-Type: application/json" -d '{"text": "I am going to see Deadpool2 on Friday."}' 

Or whatever you're sending from the database.

But you're going to do a HTTPS POST to the rest endpoint. So do that. AND get
the response, AND get the response type (200, 400, 504)
--}

data S = S { txt :: String }

instance FromRow S where
   fromRow = undefined

fetchStmt :: Int -> Int -> Query
fetchStmt n offset = undefined

-- query looks like: [sql|SELECT id,full_text FROM article WHERE id > ? LIMIT (?)|]

fetchIxArticle :: Connection -> Int -> Int -> IO (IxValue S)
fetchIxArticle conn n off = undefined

-- fetches n ar

curlCmd :: String -> IO (Status, String)
curlCmd text = undefined

-- sends text, gets a response and status

logStatus :: Connection -> (Status, String) -> IO ()
logStatus conn (stat, resp) = undefined

-- And our application that brings it all together

main' :: [String] -> IO ()
main' args = undefined

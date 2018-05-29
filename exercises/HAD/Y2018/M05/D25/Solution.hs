{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2018.M05.D25.Solution where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Network.HTTP.Conduit
import Network.HTTP.Types hiding (Query)

-- below imports available via 1HaskellADay

import Data.Logger hiding (mod)
import Data.LookupTable

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
   fromRow = S <$> field

type Conn1 = (Connection, LookupTable)

instance Show S where show = take 100 . txt

fetchStmt :: Int -> Int -> Query
fetchStmt n offset =
   Query (B.pack ("SELECT id,full_text FROM article WHERE id > " ++ show offset
                ++ " LIMIT " ++ show n))

-- query looks like:
-- [sql|SELECT id,full_text FROM article WHERE id > ? LIMIT (?)|]

fetchIxArticle :: Connection -> Int -> Int -> IO [IxValue S]
fetchIxArticle conn n = query_ conn . fetchStmt n

-- fetches n articles.

-- Question: IDs are not sequential. What is the max id of the fetch?

curlCmd :: Conn1 -> Int -> IxValue S -> IO (Status, String)
curlCmd = cc' 30 0

cc' :: Int -> Int -> Conn1 -> Int -> IxValue S -> IO (Status, String)
cc' secs tries conn n msg =
   if tries > 3 then return (Status 504 "failed", "")
   else (newManager tlsManagerSettings >>= \mgr ->
   parseRequest nerEndpoint >>= \req -> 
   let jsn = BL.pack ("{ \"text\": " ++ show (txt (val msg)) ++ " }")
       req' = req { responseTimeout = responseTimeoutMicro (secs * 1000000),
                    method = "POST",
                 -- requestHeaders = [(CI "application", "json")],
                    requestBody = RequestBodyLBS jsn } in
   httpLbs req' mgr >>= \((responseStatus &&& BL.unpack . responseBody) -> it) ->
   logStatus conn n tries msg it >>
   (if n `mod` 100 == 0
    then info conn ("NER processed " ++ show n ++ " articles")
    else return ())        >>
   return it)

-- sends text, gets a response and status

logStatus :: Conn1 -> Int -> Int -> IxValue S -> (Status, String) -> IO ()
logStatus conn n tries art@(IxV x _) (stat, resp) =
   if statusCode stat == 200
   then debug conn n x resp
   else warn conn tries n x stat resp >> cc' 30 (succ tries) conn n art >> return ()
      where debug (conn,lk) n x msg =
               roff conn lk DEBUG "Load Tester" "Y2018.M05.D25.Solution"
                    ("Row " ++ show n ++ ", entities (article " ++ show x
                            ++ "): " ++ take 200 resp)

warn :: Conn1 -> Int -> Int -> Integer -> Status -> String -> IO ()
warn (conn,lk) tries n x stat msg =
   roff conn lk WARN "Load Tester" "Y2018.M05.D25.Solution"
                    ("WARNING Row " ++ show n ++ ", code: "
                          ++ show (statusCode stat) ++ " for article " ++ show x
                          ++ ": " ++ msg)

info :: Conn1 -> String -> IO ()
info (conn,lk) = mkInfo "Load Tester" "Y2018.M05.D25.Solution" lk conn

-- And our application that brings it all together

main' :: [String] -> IO ()
main' [n, offset] =
   let n1 = read n
       off1 = read offset in
   withConnection PILOT (\conn ->
      initLogger conn >>= \lk -> 
      let log = info (conn,lk) in
      log ("Load testing API Gateway with " ++ n ++ " articles from " ++ offset) >>
      fetchIxArticle conn n1 off1 >>= \arts ->
      log ("Fetched " ++ n ++ " articles") >>
      mapM_ (\(x, art) -> curlCmd (conn, lk) x art >> threadDelay 2000) (zip [1..] arts) >>
      log ("Processed entities for " ++ n ++ " articles") >>
      log ("Max ID is " ++ show (maximum (map idx arts))))

{--
>>> main' (words "3 253013")
Stamped {stamped = Entry {sev = INFO, app = "Load Tester", mod = "Y2018.M05.D25.Solution", msg = "Load testing API Gateway with 3 articles from 253013"}, time = 2018-05-25 15:11:13.885881}
Stamped {stamped = Entry {sev = INFO, app = "Load Tester", mod = "Y2018.M05.D25.Solution", msg = "Fetched 3 articles"}, time = 2018-05-25 15:11:13.991414}
Stamped {stamped = Entry {sev = INFO, app = "Load Tester", mod = "Y2018.M05.D25.Solution", msg = "Processed entities for 3 articles"}, time = 2018-05-25 15:11:51.739545}
Stamped {stamped = Entry {sev = INFO, app = "Load Tester", mod = "Y2018.M05.D25.Solution", msg = "Max ID is 253016"}, time = 2018-05-25 15:11:51.743727}
--}

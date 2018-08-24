{-# LANGUAGE OverloadedStrings #-}

module Y2018.M08.D22.Solution where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Maybe (fromJust)

import Network.HTTP.Conduit

import System.Directory (listDirectory)

-- Today's Haskell problem. We have a set of JSON files at

exDir :: FilePath
exDir = "Y2018/M08/D22/files/"

-- what are the names of these JSON files?

jsonfilenames :: FilePath -> IO [FilePath]
jsonfilenames = fmap (filter (isSuffixOf ".json")) . listDirectory

{--
>>> files <- jsonfilenames exDir 
>>> files
["e40aab36-8b8c-11e8-a008-9f886eb04e8d.json",
 "38bf927e-9b3a-11e8-bdb8-cf5b8181e232.json",
 "23811e14-a53a-11e8-a333-73aec78d8ae9.json",
 "cea3b763-742b-5cef-85f0-c5e63ca76324.json",
 "ace58b44-83a8-11e8-8cde-9fb498e2ca09.json"]

For each of the files in jsonfilenames, read in the JSON, then write out
the JSON as:

{
    "article_id": (the file name (without the extension)),
    "entities": (the contents of the JSON file)
}

--}

data Entities = Art { artId :: String, entities :: Map String Value }

instance ToJSON Entities where
   toJSON (Art idx ents) = object ["article_id" .= idx, "entities" .= ents]

restructureJSON :: FilePath -> FilePath -> IO ByteString
restructureJSON dir file = BL.readFile (dir ++ file) >>=
                           return . encodePretty . Art (conv file) . map2map

map2map :: ByteString -> Map String Value
map2map = fromJust . decode

conv :: String -> String
conv = fst . break (== '.')

{--
>>> restructureJSON exDir (head files)
{
    "entities": {
        "farm fresh": {
            "related_content": [
                {
                    "uid": "d5642820-a9cf-5d9d-8c6f-06ebfdb7e573",
                    "weighted_score": 6.582166813644148e15
                },
...
--}

{-- BONUS -----------------------------------------------------------------

Set up a REST endpoint, it can be as simple as an echo chamber, send the
restructured JSON to that REST endpoint for processing. For me, I have a REST
endpoint that saves these article analyses to a database. You do what you like.
--}

type Endpoint = FilePath

url :: Endpoint
url = "https://some-url"

-- remove me: url = "https://lj4yu80wdk.execute-api.us-east-1.amazonaws.com/entity-storer"


processJSON :: Endpoint -> FilePath -> IO ()
processJSON url jsonfile =
   restructureJSON exDir jsonfile >>= \json ->
   newManager tlsManagerSettings  >>= \mgr ->
   parseRequest url               >>= \req ->
   let req' = req { method = "POST", requestBody = RequestBodyLBS json } in
   httpLbs req' mgr >>=
   print

-- hint: this make look a lot like a POST request from curl.

-- are there any results returned?

{--
>>> processJSON url (head files)
Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, 
          responseVersion = HTTP/1.1, 
          responseHeaders = [("Date","Wed, 22 Aug 2018 10:01:29 GMT"),
          ("Content-Type","application/json"),("Content-Length","4"),
          ("Connection","keep-alive"),
          ("x-amzn-RequestId","5705b2bb-a5f2-11e8-a375-9b924bcfb38a"),
          ("Access-Control-Allow-Origin","*"),
          ("x-amz-apigw-id","MBUi_EtjoAMF4Iw="),
          ("X-Amzn-Trace-Id","Root=1-5b7d3479-0a5c4eb271a3447e7ec34294;Sampled=0")], 
          responseBody = "null", responseCookieJar = CJ {expose = []}, 
          responseClose' = ResponseClose}

and

>>> mapM_ (processJSON url) (tail files)

inserts the rest.
--}

module Wikidata.Query.Endpoint where

{--
WHEW! Has querying the wikidata endpoint gotten easier or what?!?

Just submit your SPARQL query using the sparql function and you'll ge back 
a ByteString response as JSON. Decoding the JSON is on you.
--}

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP (urlEncode)

endpoint :: FilePath
endpoint = "https://query.wikidata.org/sparql?format=json&query="

sparql :: String -> IO ByteString
sparql = simpleHttp . (endpoint ++) . urlEncode

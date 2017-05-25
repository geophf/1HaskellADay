module Graph.Query where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP

import Graph.JSON.Cypher

{-- A solution to the problem posted at http://lpaste.net/7339795115373756416
@1HaskellADay solution for 2016-03-21

Last week we looked at data extracted from a (graph) database. This week, we are
going to have some FUN! We will interact with an endpoint to a graph database
on AWS using Haskell!

Yippee!

So, last week we compiled a bunch of symbols (strings), to, well, a bunch of
symbols (enumerated, bounded, indexible values of a type).

This week, we're going to continue that work, and actually solve problems
here against a running database.

First, let's get the connection URI:
--}

type Endpoint = String

{--
Sweet. Now that we have that, let's make a REST-call to that endpoint to get
a set of the distinct stock symbols stored in that database. The Cypher query
to do that is:

queryStocks :: Cypher
queryStocks = "match (s:SECURITY) return distinct s.symbol"

You can format a Cypher-query into JSON with Data.Relation imported above

... but how to get JSON back from that (JSON) query from the endpoint?

We'll look at that today.

-- ref: Analytics.Trading.Web.Upload.Cypher -- http://lpaste.net/4270680441352093696

The above referenced module loads data into the graph database, but we actually
want to extract data from the database using the queryStocks value, so the
referred module is no help at all! Shoot!

How about Network.HTTP?

And, something I learned when delivering a graph-database-to-web-app contract,
you have to dig deeper into the endpoint. To complete a query as a transaction
you have to go all the way down to:
--}

transaction :: String
transaction = "db/data/transaction/commit"

{-- 
Send the Cypher query as JSON to the endpoint and get the response back as
a String. We'll look at parsing out the response as JSON tomorrow.
--}

getGraphResponse :: Foldable t => Endpoint -> t (Cypher) -> IO String
getGraphResponse transactionEndpoint queries =
   simpleHTTP (postRequestWithBody transactionEndpoint "application/json"
                       . BL.unpack $ cypher2JSON queries) >>= getResponseBody

{--
*Main> getGraphResponse (endpoint ++ ('/':transaction)) [queryStocks] ~>
"{\"results\":[{\"columns\":[\"s.symbol\"],\"data\":[{\"row\":[\"RDS.B\"]},
...{\"row\":[\"AAAP\"]},{\"row\":[\"CPGX\"]}]}],\"errors\":[]}"

YAY!
--}

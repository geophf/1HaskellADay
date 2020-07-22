module Y2020.M07.D21.Solution where

import Network.HTTP.Conduit
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy.Char8 as L

{--
Today, we're going to make a GET request to a REST endpoint, and simply
GET the data from that API back.

GET ... IT?

The endpoint we are calling GETs a list of REST endpoints to public data. You
may explore those endpoint as you desire, this exercise is simply to GET those
public API endpoints.

Luckily the list of public API endpoints has a public API endpoint. It is:
--}

publicAPIEndpoint :: FilePath
publicAPIEndpoint = "https://api.publicapis.org/entries"

-- call that endpoint and return the results

getInfos :: FilePath -> IO ()
getInfos = simpleHttp >=> L.putStrLn

{--

--}

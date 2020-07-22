module Y2020.M07.D22.Exercise where 

{--
So, day before yesterday, we parsed a JSON file...
So, yesterday, we contacted a REST endpoint with a GET request and got a
response-as-string-but-it's-really-JSON.

So, where two stream join, you fish.

I learned that from Hannibal Lecter.

Today's Haskell problem: go to the endpoint from yesterday. You 'know' we
have a list response of 642 entries. Now, divide that response into two
result sets: 

Those that are HTTPS endpoints and those that are not.


Also, bonus, it'd be nice if the APIs were searchable by name (so: Map).
--}

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

import Y2020.M07.D21.Exercise

type Name = String
type Description = String
type IsHttps = Bool
type URL = FilePath
type Category = String

data API = API Name Description IsHttps URL Category
   deriving (Eq, Ord, Show)

instance FromJSON API where
   parseJSON = undefined

partitionedAPIs :: ByteString -> Map Name API
partitionedAPIs response = undefined

-- How many public APIs are HTTP? How many public APIs are HTTPS?

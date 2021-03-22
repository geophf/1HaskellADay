module Y2021.M03.D22.Exercise where

{--
So, you're given JSON-as-string (because don't even get me started)

(Work! am I right, fam?) 

And you need to add a field to that JSON, sometime-ish, because you're testing,
and stuff!

Do that.
--}

import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString.Lazy.Char8 as BL

json :: String
json = "{ \"a\": 3, \"b\": 4, \"c\": 17 }"

-- now add "d": 42 because "d" stands for "Douglas Adams," apparently.

updateJson :: String -> String
updateJson json = undefined

-- make sure your output is valid JSON! You may use Data.Aeson to do that.

validateJson :: String -> Maybe Value
validateJson outputJson = undefined

-- I mean, you could even TYPE the inputs and outputs, so that when your
-- data scientist hands you 100 megabytes of JSON that he generated from 
-- python, and reversed the key and the value in one of the many (many (MANY!))
-- records, the type-system will catch that, and not you having an argument
-- with said data scientist who claims nothing is wrong until a day later
-- one of you (meaning: you) finds the issue.

-- but that's just me.
-- /rant.

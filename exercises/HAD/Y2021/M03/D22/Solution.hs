{-# LANGUAGE OverloadedStrings #-}

module Y2021.M03.D22.Solution where

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

import Data.Map (Map)
import qualified Data.Map as Map

jsonVal :: String
jsonVal = "{ \"a\": 3, \"b\": 4, \"c\": 17 }"

-- now add "d": 42 because "d" stands for "Douglas Adams," apparently.

updateJson :: String -> Maybe String
updateJson json =

{--
One way to do this, the way I did it at work, is to back-track over the string,
delete the final '}', insert the key-value pair, then re-add the '}'.

But why do that, when you can convert it into a type-value, like say: a Map,
then add the key-value pair to the map, safely, and then regenerate the 
string from the (BONUS!) already-valid JSON.
--}

   let thing = (decode (BL.pack json)) :: Maybe (Map String Int) in
   BL.unpack . encode . Map.insert "d" 42 <$> thing

{--
>>> let (Just res) = updateJson jsonVal 
>>> putStrLn res
{"a":3,"b":4,"c":17,"d":42}
--}

-- make sure your output is valid JSON! You may use Data.Aeson to do that.

validateJson :: String -> Maybe Value
validateJson = decode . BL.pack

{--
>>> validateJson res
Just (Object (fromList [("a",Number 3.0),("d",Number 42.0),
                        ("b",Number 4.0),("c",Number 17.0)]))
--}

-- I mean, you could even TYPE the inputs and outputs, so that when your
-- data scientist hands you 100 megabytes of JSON that he generated from 
-- python, and reversed the key and the value in one of the many (many (MANY!))
-- records, the type-system will catch that, and not you having an argument
-- with said data scientist who claims nothing is wrong until a day later
-- one of you (meaning: you) finds the issue.

-- but that's just me.
-- /rant.

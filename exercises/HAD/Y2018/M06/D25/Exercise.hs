{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D25.Exercise where

{--
You have some JSON. Parse that puppy in.

Now, fun, translate the JSON into something else. Create an article id, populate
that value in the parsed-in JSON and print out the result, prettily.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty

exDir, errFile :: FilePath
exDir = "Y2018/M06/D25/"
errFile = "ErrorLogging.json"

data ErrorReport = ER { entityId, articleId, entity, error :: String }
   deriving (Eq, Show)

instance FromJSON ErrorReport where
   parseJSON json = undefined

readError :: FilePath -> IO ErrorReport
readError file = undefined

artId :: String
artId = "c9d40d94-6b24-11e8-bea5-9b00fce84fd4"

instance ToJSON ErrorReport where
   toJSON err = undefined

printError :: ErrorReport -> IO ()
printError err = undefined

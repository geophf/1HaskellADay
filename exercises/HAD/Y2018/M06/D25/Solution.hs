{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D25.Solution where

{--
You have some JSON. Parse that puppy in.

Now, fun, translate the JSON into something else. Create an article id, populate
that value in the parsed-in JSON and print out the result, prettily.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

exDir, errFile :: FilePath
exDir = "Y2018/M06/D25/"
errFile = "ErrorLogging.json"

data ErrorReport = ER { entityId :: Integer, articleId, url, error :: String }
   deriving (Eq, Show)

-- of course I had to change entityId type to Integer for this to work, oh, well

instance FromJSON ErrorReport where
   parseJSON (Object o) =
      ER <$> o .: "EntityId" <*> o .: "ArticleId" <*> o .: "URL"
         <*> o .: "Error"

readError :: FilePath -> IO ErrorReport
readError = fmap (fromJust . decode) . BL.readFile

{--
>>> err <- readError (exDir ++ errFile)
>>> err
ER {entityId = 1, articleId = "",
    url = "https://en.wikipedia.org/wiki/Battle_of_Midway", error = "Wikipedia"}
--}

artId :: String
artId = "c9d40d94-6b24-11e8-bea5-9b00fce84fd4"

instance ToJSON ErrorReport where
   toJSON (ER e a u r) =
      object ["EntityId" .= e, "ArticleId" .= a, "URL" .= u, "Error" .= r]

printError :: ErrorReport -> IO ()
printError = BL.putStrLn . encodePretty

{--
>>> newErr = err { articleId = artId }
>>> printError newErr 
{
    "Error": "Wikipedia",
    "URL": "https://en.wikipedia.org/wiki/Battle_of_Midway",
    "EntityId": 1,
    "ArticleId": "c9d40d94-6b24-11e8-bea5-9b00fce84fd4"
}
--}

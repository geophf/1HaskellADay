{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D18.Solution where

{--
Yesterday, we created the links for requesting an oembed component, today we
will receive an oembed request and then respond.

Reminder, the request looks something like

http://127.0.0.1:8080/services/oembed/?format=json&url=http%3A//127.0.0.1%3A8080/Y2017/M12/D13/goatz.jpg

And today, we will provide the response, that will look something like:

{
	"version": "1.0",
	"type": "photo",
	"width": 240,
	"height": 160,
	"title": "goatz",
	"url": "http://127.0.0.1:8080/Y2017/M12/D13/goatz.jpg"
	"author_name": "geophf",
	"author_url": "http://logicaltypes.blogspot.com"
}

Okay, let's get to it!
--}

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL

-- below import available via 1HaskellADay git repository

import Y2017.M12.D15.Solution hiding (main')

-- let's say we magically receive the GET values as arguments to main, so let's
-- not worry about parsing the request URL at this time.

-- We do need a structure that captures the above response JSON however

data OEmbedResponse =
   OEResp { kind, title :: String, oeurl :: FilePath, width, height :: Int }
      deriving (Eq, Show)

instance ToJSON OEmbedResponse where
   toJSON resp = object [("version", "1.0"), "type" .= kind resp,
                         "width" .= width resp, "height" .= height resp,
                         "title" .= title resp, "url" .= oeurl resp,
                         ("author_name", "geophf"),
                         ("author_url", "http://logicaltypes.blogspot.com")]

{--
>>> BL.putStrLn (encodePretty (OEResp "photo" "goatz" "url" 123 345))
{
    "height": 345,
    "author_name": "geophf",
    "url": "url",
    "width": 123,
    "version": "1.0",
    "title": "goatz",
    "type": "photo",
    "author_url": "http://logicaltypes.blogspot.com"
}
--}

-- So now we generate a response from the values we received from the request:

data OEmbedRequest = OEReq { format :: Maybe Format, requrl :: FilePath }
   deriving (Eq, Show)

-- today we are only responding in JSON. We'll look at XML response tomorrow

respondJSON :: OEmbedRequest -> OEmbedResponse
respondJSON (OEReq _form req) =
   OEResp "photo" "goat pic" req wid hei
      where wid = 512  -- width of the photo, somehow
            hei = 256  -- height of the photo, somehow

{--
>>> respondJSON (OEReq (Just JSON) "http://goats.com")
OEResp {kind = "photo", title = "goat pic", oeurl = "http://goats.com", width = 512, height = 256}
--}

-- but how do we create the request value?

strs2Req :: [String] -> OEmbedRequest
strs2Req [justurl] = OEReq Nothing justurl
strs2Req [_json, url] = OEReq (Just JSON) url

{--
The argument are arranged by the caller and are, by convention

oeResp [format] <goat-url>

If format is there, it will be "json"; if not, then just the goat-url is
present. Either way, you can construct the OEmbedRequest object from that.

>>> strs2Req ["http://goats.com"]
OEReq {format = Nothing, requrl = "http://goats.com"}
>>> strs2Req ["json", "http://goats.com"]
OEReq {format = Just json, requrl = "http://goats.com"}
--}

{-- BONUS -----------------------------------------------------------------


Build an application that, given the arguments of the optional formatting
type and the url of the asset, returns the OEmbedResponse as JSON.
--}

main' :: [String] -> IO ()
main' args = if len < 1 || len > 2 then usage else
   BL.putStrLn (encodePretty (respondJSON (strs2Req args)))
      where len = length args

usage :: IO ()
usage = putStrLn (unlines ["","oeResp [json] <url>", "",
   "\twhere json indicates that the response will be in json",
   "\t      url  is the url of the asset to be embed",""])

{-- BONUS-BONUS -----------------------------------------------------------

May not be your your cuppa tea, but give this a go. Create a REST server
that takes the oembed request (as formatted yesterda) and returns the oembed
response. You can go full-Haskell with this, if you'd like, by creating a 
Haskell servlet, or you can use whatever webby-resty language you prefer.
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D18.Exercise where

{--
Yesterday, we created the links for requesting an oembed component, today we
will receive an oembed request and then respond.

Reminder, the request looks something like

http://127.0.0.1:8080/services/oembed/?format=json&url=http%3A//127.0.0.1%3A8080/Y2017/M12/D15/goatz.jpg

And today, we will provide the response, that will look something like:

{
	"version": "1.0",
	"type": "photo",
	"width": 240,
	"height": 160,
	"title": "goatz",
	"url": "http://127.0.0.1:8080/Y2017/M12/D15/goatz.jpg"
	"author_name": "geophf",
	"author_url": "http://logicaltypes.blogspot.com"
}

Okay, let's get to it!
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty

-- below import available via 1HaskellADay git repository

import Y2017.M12.D15.Exercise

-- let's say we magically receive the GET values as arguments to main, so let's
-- not worry about parsing the request URL at this time.

-- We do need a structure that captures the above response JSON however

data OEmbedResponse = SomethingLikeTheAbove
   deriving (Eq, Show)

instance ToJSON OEmbedResponse where
   toJSON response = undefined

-- So now we generate a response from the values we received from the request:

data OEmbedRequest = OEReq { format :: Maybe Format, url :: FilePath }
   deriving (Eq, Show)

-- today we are only responding in JSON. We'll look at XML response tomorrow

respondJSON :: OEmbedRequest -> OEmbedResponse
respondJSON req = undefined

-- but how do we create the request value?

strs2Req :: [String] -> OEmbedRequest
strs2Req arguments = undefined

{--
The argument are arranged by the caller and are, by convention

oeResp [format] <goat-url>

If format is there, it will be "json"; if not, then just the goat-url is
present. Either way, you can construct the OEmbedRequest object from that.
--}

{-- BONUS -----------------------------------------------------------------

Build an application that, given the arguments of the optional formatting
type and the url of the asset, returns the OEmbedResponse as JSON.
--}

main' :: [String] -> IO ()
main' args = undefined

{-- BONUS-BONUS -----------------------------------------------------------

May not be your your cuppa tea, but give this a go. Create a REST server
that takes the oembed request (as formatted yesterda) and returns the oembed
response. You can go full-Haskell with this, if you'd like, by creating a 
Haskell servlet, or you can use whatever webby-resty language you prefer.
--}

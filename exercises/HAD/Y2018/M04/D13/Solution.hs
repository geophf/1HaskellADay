{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D13.Solution where

{--
So, we're going to download packets from WordPress until we get tired of it
(that is to say, until we exhaust the download). How do we knw when we end?
Well, for WordPress, what I see is that a packet is a set of articles or it's
this message:

{"code":"rest_post_invalid_page_number",
 "message":"The page number requested is larger than the number of pages available.",
 "data":{"status":400}}

or, that is to say, something other than a set of articles. Let's do this.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Y2018.M04.D11.Solution

-- our REST endpoint is of the form:

endpoint :: PageNumber -> FilePath
endpoint = ("https://worldpolicy.org/wp-json/wp/v2/posts?per_page=100&page=" ++)
   . show

-- now we ... 'know' that there are less than 1000 pages in the archive.

-- Build a system that snarfs the articles from the endpoint and dumps them
-- locally

data Packet a = Pack [a]
   deriving Show

instance ToJSON Protec where
   toJSON (Pro n c) = object ["page" .= n, "count" .= c]

pack2protec :: Packet Value -> PageNumber -> Protec
pack2protec (Pack arts) n = Pro n (length arts)

instance FromJSON a => FromJSON (Packet a) where
   parseJSON o = Pack <$> parseJSONList o

readPacket :: PageNumber -> IO (Either (Packet Value) String)
readPacket = rp' 60

-- either gives a packet or an 'error' string showing what it couldn't parse

rp' :: Int -> Int -> IO (Either (Packet Value) String)
rp' secs offset =
   newManager tlsManagerSettings >>= \mgr ->
   parseRequest (endpoint offset) >>= \req ->
   let req' = req { responseTimeout = responseTimeoutMicro (secs * 1000000) } in
   httpLbs req' mgr >>= return . eitherify decode . responseBody

eitherify :: (ByteString -> Maybe (Packet Value)) -> ByteString
          -> Either (Packet Value) String
eitherify f str = case f str of
   Just pack -> Left pack
   Nothing   -> Right (BL.unpack str)

-- Read in all the packets and store them locally. We'll upload them to the
-- database ... tomorrow?

{--
>>> readPacket 1000
Right "{\"code\":\"rest_post_invalid_page_number\",\"message\":\
      "The page number requested is larger than the number of pages available.\",
       \"data\":{\"status\":400}}"
>>> readPacket 1
Left ...

So, yes
--}

type Tries = Int

packetReader :: PageNumber -> PageNumber -> Tries -> IO ()
packetReader n m t =
   if n > m
   then putStrLn "Done"
   else if t > 3
        then error ("Tried three times at offset " ++ show n ++ "; quitting")
        else readPacket n >>= either (savePacket n m) (errOut n m t)

savePacket :: PageNumber -> PageNumber -> Packet Value -> IO ()
savePacket n m pak =
   BL.writeFile ("packet" ++ show n ++ ".json")
                (encodePretty (pack2protec pak n)) >>
   packetReader (succ n) m 0

errOut :: PageNumber -> PageNumber -> Tries -> String -> IO ()
errOut n m t msg =
   putStrLn ("Failed on on page " ++ show n ++ ", try: " ++ show t ++ ", with "
             ++ "string " ++ msg) >>
   packetReader n m (succ t)

{--
>>> packetReader 1 2 0
Done

$ ls -l p*
-rw-r--r--  1 geophf  staff  1201664 Apr 13 18:56 packet1.json
-rw-r--r--  1 geophf  staff  1241316 Apr 13 18:56 packet2.json

Whoa! These packets are coming out at 1.2 megabytes each!
--}

module Y2018.M04.D13.Exercise where

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
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Y2018.M04.D11.Exercise (PageNumber)

-- our REST endpoint is of the form:

endpoint :: PageNumber -> FilePath
endpoint pn = "https://worldpolicy.org/wp-json/wp/v2/posts?per_page=100&page="
   ++ show pn

-- now we ... 'know' that there are less than 1000 pages in the archive.

-- Build a system that snarfs the articles from the endpoint and dumps them
-- locally

data Packet a = Pack [a]
   deriving Show

-- we're ... 'hoping' the a-type value resolves, eventually to an Article value,
-- but we also want to store its raw (JSON) form in a data table.

instance FromJSON a => FromJSON (Packet a) where
   parseJSON (Object o) = undefined

readPacket :: PageNumber -> PageNumber -> IO (Either (Packet Value) String)
readPacket start end = undefined

-- either gives a packet or an 'error' string showing what it couldn't parse

-- you may wish to look at Y2018.M01.D16.Exercise for dealing with timeouts
-- and retries. But this time: how many retries to you try?

-- Read in 5 packets and store them locally. We'll upload them to the
-- database ... tomorrow?

type Tries = Int

packetReader :: PageNumber -> PageNumber -> Tries -> IO ()
packetReader start finish ntries = undefined

-- packetReader calls either savePacket (Left Packet) or errOut (Right String)
-- or quits if the tries are exhausted

savePacket :: PageNumber -> PageNumber -> Packet Value -> IO ()
savePacket n m pak = undefined

-- savePacket's continuation is packetReader with the next page and 0 tries

errOut :: PageNumber -> PageNumber -> Tries -> String -> IO ()
errOut n m t msg = undefined

-- errOut prints an error message then tries packetReader n m (succ t) again 

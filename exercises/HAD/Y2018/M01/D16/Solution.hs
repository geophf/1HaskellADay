module Y2018.M01.D16.Solution where

{--
Now that we have packet information, ... well, the funny thing is, we've always
had packet information, it's just that I'm looking at continuation and error-
recovery, but that's down the road, not for today ...

So, now that we have packet information, let's do the following.

From a packet p0, and a REST endpoint r, load in the NEXT packet of articles.

How do we do this?

The rest endpoint:
--}

import Data.Aeson (decode)

import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Solution (Packet,next,rows)

endpoint :: FilePath
endpoint = "https://pilotonline.com/search/?sd=desc&s=start_time&f=json&t=article&nfl=ap&l=100&o="

{--
Those are a lot of arguments, but we don't need to worry about most of them.

The one that concerns us is the argument 'o' which stands for 'offset.'

For us, the offset is either 0 if we're starting with a new archive or
the offset is from the most recent, or only active audit log entry:

For now just pick an offset so we can load in a packet:
--}

readPacket :: Integer -> IO (Maybe Packet)

{--
here is the plain-jain simpleHttp answer to the exercise:

readPacket offset = simpleHttp (endpoint ++ show offset) >>= return . decode
--}

-- here's readPacket with a timeout:

readPacket = rp' 60

rp' :: Int -> Integer -> IO (Maybe Packet)
rp' secs offset =
   newManager tlsManagerSettings >>= \mgr ->
   parseRequest (endpoint ++ show offset) >>= \req ->
   let req' = req { responseTimeout = responseTimeoutMicro (secs * 1000000) } in
   httpLbs req' mgr >>= return . decode . responseBody

-- What is the value of next for the packet read? How many blocks were read?

{--
>>> Just pac <- readPacket 100
>>> next pac
200
>>> length (rows pac)
100
--}

module Y2018.M01.D16.Exercise where

{--
Now that we have packet information, ... well, the funny thing is, we've always
had packet information, it's just that I'm looking at continuation and error-
recovery, but that's down the road, not for today ...

So, now that we have packet information, let's do the following.

From a packet p0, and a REST endpoint r, load in the NEXT packet of articles.

How do we do this?

The rest endpoint:
--}

import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise (Packet)

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
readPacket offset = undefined

-- What is the value of next for the packet read? How many blocks were read?

{-- BONUS -----------------------------------------------------------------

What if it takes a long time to get a response? The default timeout is 5
seconds. What if it takes up to 60 seconds? Write a function that modifies
the default timeout to allow for a longer response time.
--}

rp' :: Int -> Integer -> IO (Maybe Packet)
rp' secs offset = undefined

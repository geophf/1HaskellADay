module Y2018.M01.D18.Exercise where

{--
Do we have everything? I think we have everything.

Given an active audit record in the database, load in a set of packets from the
REST endpoint and upload the processed articles to the database.
--}

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise (Packet,next)
import Y2018.M01.D04.Exercise (pa,storeAncilliary)
import Y2018.M01.D15.Exercise (etl)
import Y2018.M01.D16.Exercise (readPacket)
import Y2018.M01.D17.Exercise (fetchActiveAuditEntry, offset)

-- okay: read in the auditing information to get the offset, then read and
-- store n packets from that offset:

loop :: Integer -> Int -> IO ()
loop offset n = undefined

-- remember to update the offset with each iteration of the loop
-- remember to give loop the offset from the audit log to start it off.

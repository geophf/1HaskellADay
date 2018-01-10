{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D10.Exercise where

{--
Today's Haskell exercise is a bit of a warming-up exercise. Today we'll store
packet information in the packet table (the structure of the table matching
Packet (see Y2017.M12.D20.Exercise), and also write the fetch function that
retrieves packet information from the database from a given id (which we will
provide later).

We do this for auditing purposes. We wish to store not only the articles that
we store, but also information about what we stored and when. Storing packet
information is a piece of that puzzle.

... hm. Actually, I don't think a fetch function is necessary given how I'm
planning auditing. We shall see. For today, just write the toRow function
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise -- for Packet

instance ToRow Packet where
   toRow pack = undefined

{--
Using Y2017.M12.D20.Exercise.readSample, load in the packet and store its
information.
--}

{-- BONUS -----------------------------------------------------------------

Rewrite the etl-process to store the packet information (that is: don't discard
packet information anymore) along with the articles.
--}

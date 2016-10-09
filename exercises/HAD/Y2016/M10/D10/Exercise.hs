module Y2016.M10.D10.Exercise where

import Data.Time.Clock.POSIX

-- below imports available as a cabal install

import Data.Aeson
import Network.HTTP.Conduit

{--
So, last week you created a webservice that showed block information for the
latest block, then refined that to show block information for some requested
block.

The problem here is that: do you know a block hash off the top of your head?

No, you don't.

So it's best to survey block information by some criteria, such as by address,
or by day, or by pool, or by height.

Let's survey blocks by day today.

A sample set of block (meta-)information returned from a block at a specified
time (the POSIX-integer representation of time) is located at this directory:

blocks.json

or at this URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D10/blocks.json

Create a structure that accepts these data, then, you know: read it in.
--}

data BlockInfo = AStructureYouCreate

instance FromJSON BlockInfo where
   parseJSON = undefined

-- now read in the block info for some time from the blockchain.info api

blocksAtTimeURL :: Integer -> FilePath
blocksAtTimeURL t = "https://blockchain.info/blocks/" ++ show t ++ "?format=json"

readBlockInfo :: POSIXTime -> IO [BlockInfo]
readBlockInfo t = undefined

-- now read in the block info for today/the current POSIX time

readTodaysBlockInfo :: IO [BlockInfo]
readTodaysBlockInfo = undefined

{-# LANGUAGE OverloadedStrings #-}

module Data.BlockChain.Block.BlockInfo where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX

-- below imports available via cabal install

import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)

-- below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.Types
import Data.BlockChain.Block.Utils

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

data BlockInfo = BlockInfo { height :: Integer, hashId :: Hash, time :: Integer }
   deriving Show

instance Sheesh BlockInfo where
   hash = hashId

instance FromJSON BlockInfo where
   parseJSON (Object o) =
      BlockInfo <$> o .: "height" <*> o .: "hash" <*> o .: "time"

data Blocks = Blocks { blocks :: [BlockInfo] }
   deriving Show

instance FromJSON Blocks where
   parseJSON (Object o) = Blocks <$> o .: "blocks"

scanBlocks :: ByteString -> Maybe Blocks
scanBlocks = decode

{--
*Y2016.M10.D10.Solution> fmap scanBlocks $ BL.readFile "Y2016/M10/D10/blocks.json" 
Just (Blocks {blocks = [BlockInfo {height = 166107, hash = "0000...fe0a", time = 1328830483},
                        BlockInfo {height = 166104, hash = "0000...7995", time = 1328828041}]})

I know I have an Int to time converter here somewhere. Time to make an Utils
module!

... yup, it's in Data.BlockChain.Block.Graphs ... moving it
--}

-- now read in the block info for some time from the blockchain.info api

blocksTodayURL :: FilePath
blocksTodayURL = "https://blockchain.info/blocks?format=json"

{--
bleh: bad url? obe url? idk
readBlockInfo :: POSIXTime -> IO Blocks
readBlockInfo = fmap (fromJust . scanBlocks) . simpleHttp . blocksAtTimeURL . floor
--}

-- now read in the block info for today/the current POSIX time

readTodaysBlockInfo :: IO Blocks
readTodaysBlockInfo = fmap (fromJust . scanBlocks) $ simpleHttp blocksTodayURL

-- *Y2016.M10.D10.Solution> readTodaysBlockInfo ~> blks ~> length (blocks blks) ~> 88

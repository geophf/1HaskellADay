{-# LANGUAGE OverloadedStrings #-}

module Y2016.M10.D06.Solution where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import Snap.Core -- if you want to use the Snap Framework 
import Snap.Http.Server

-- below modules available from 1HaskellADay git repository

import Control.Scan.CSV (rend)

import Data.BlockChain.Block.Blocks (Block, rawBlockURL, readBlock, tx)
import Data.BlockChain.Block.Graphs
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.BlockChain.Block.Utils

import Data.Monetary.BitCoin
import Data.Tree.Merkle
import Data.XHTML

{--
So, we made a web service! YAY!

Okay, but it's a little clunky, so some housekeeping:

1. instead of latest block, have a GET-service that accepts a hash of a block
   and returns the transactions for that block.
--}

transactionsForBlock :: Hash -> IO [Transaction]
transactionsForBlock = fmap tx . readBlock rawBlockURL

{--
2. And with that, show the block hash ID when you list all the hashes of the 
   transactions of that block
--}

-- so this would be a redefinition of yesterday's top function

top :: Snap ()
top = do
   id <- getParam "hash"
   blk  <- liftIO $ readBlock rawBlockURL (B.unpack (fromJust id))
   writeBS . B.pack . show . rep . Doc [Elt "title" [] [S "Block Transactions"]]
           . (Elt "h2" [] [S ("Transactions for Block " ++ hash blk)]:) . pure
           . tabulate [Attrib "border" "1"] [thdrs (words "Hash BTC")]
           . map (transaction2Sum (hash blk)) $ tx blk

data XSummary = XSum Hash Hash BitCoin

transaction2Sum :: Hash -> Transaction -> XSummary
transaction2Sum blk =
   uncurry (XSum blk) . (hash &&& val2BTC . sum . map value . out)

serverURL :: String
serverURL = "http://127.0.0.1:8000/"

tradeLink :: XSummary -> Element
tradeLink (XSum blk trd _) =
   Elt "a" [Attrib "href" (serverURL ++ "trade/" ++ blk ++ ('|':trd))] [S trd]

instance Rasa XSummary where
   printRow x@(XSum _ _ btc) = tr [E $ tradeLink x, S $ show btc]

{--
3. Showing the transaction as raw text is yucky. We have this construct called
   Trade that distills the Transaction information. Have a nice HTML-ish-like
   representation of a trade so my eyes don't cross when I scan a transaction.
--}

trade2HTML :: Transaction -> Element
trade2HTML tx = Elt "table" [Attrib "border" "1"]
   (map E [Elt "tr" [Attrib "bgcolor" "CCFFCC"]
             [E $ Elt "th" [Attrib "colspan" "2"]
                           [S ("Transaction " ++ hash tx)]],
    thdrs (words "From To"),

-- now, check this out! A table of tables!

     tr (map E [
     tabulate [Attrib "border" "1"] [thdrs ["Address"]] (inputs tx),
     tabulate [Attrib "border" "1"] [thdrs (words "Address BTC")] (out tx)])])

instance Rasa Input where
   printRow = tr . pure . maybe startElt addrElt . prevOut

startElt :: Content
startElt = E (Elt "b" [] [S "Start"])

addrElt :: Output -> Content
addrElt = maybe startElt S . addr

instance Rasa Output where
   printRow outie = tr [addrElt outie, S . show . val2BTC $ value outie]

{--
4. Now we need a Merkle tree-service.

4.a. For a block, give the hash of the root of the merkle tree of that block
     of transactions as a REST GET service
--}

rootHash :: MerkleTree a -> Hash
rootHash tree = undefined

{--
4.b. Given any hash of a Merkle node, give the left and right nodes' hashes
--}

childHashes :: MerkleTree a -> Hash -> (Hash, Hash)
childHashes tree hash = undefined

-- and to kick off the webservice:

main :: IO ()
main = quickHttpServe (
       route [("block/:hash", top),
              ("trade/:twohashes", trade)])

trade :: Snap ()
trade =
   getParam "twohashes" >>= \(Just twoHashes) ->
   let [blkid,trd] = rend '|' (B.unpack twoHashes) in
   do  blk <- liftIO $ readBlock rawBlockURL blkid
       writeBS . B.pack . show . rep . Doc [Elt "title" [] [S "Trade"]]
               . pure . trade2HTML . findTrade trd $ tx blk

findTrade :: Hash -> [Transaction] -> Transaction
findTrade h [] = error ("Could not find trade hash " ++ h)
findTrade h (tx:s) = if hash tx == h then tx else findTrade h s

{--
Running this webservice (you know, after you build and deploy it) gets the
results show in the screen captures in this directory.

Now, how did I know I wanted 

block 0000000000000000015336dfde93a5a10e159cae7a09abcc362a17f6190262ee

particularly?

That's an exercise for another day

(Y2016.M10.D12.Exercise, in fact)
--}

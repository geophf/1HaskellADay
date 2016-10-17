module Data.BlockChain.Block.Web where

-- gives web-view to block structures

import Control.Arrow ((&&&))
import Data.Time.LocalTime

-- below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.BlockInfo hiding (time)
import Data.BlockChain.Block.Blocks hiding (time)
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.BlockChain.Block.Utils

import Data.Monetary.BitCoin

import Data.XHTML

serverURL :: String
serverURL = "http://127.0.0.1:8000/"

link' :: String -> String -> Hash -> Element
link' branch link hash =
   Elt "a" [Attrib "href" (serverURL ++ branch ++ ('/':link))] [S hash]

link :: String -> Hash -> Element
link branch hash = link' branch hash hash

instance Rasa BlockInfo where
   printRow (BlockInfo _ h t) =
      Elt "tr" [] (map (E . Elt "td" [] . pure)
                       [E (link "block" h), S . show $ est2time t])

-- TRANSACTIONS -------------------------------------------------------------

-- there are two views on transactions.

-- 1. the detail view of the buyers and sellers (the trade):

instance Rasa Input where
   printRow = tr . pure . maybe startElt addrElt . prevOut

startElt :: Content
startElt = E (Elt "b" [] [S "Start"])

addrElt :: Output -> Content
addrElt = maybe startElt S . addr

instance Rasa Output where
   printRow outie = tr [addrElt outie, S . show . val2BTC $ value outie]

trade2HTML :: Transaction -> Element
trade2HTML tx = Elt "table" [Attrib "border" "1"]
   (map E [Elt "tr" [Attrib "bgcolor" "CCFFCC"]
             [E $ Elt "th" [Attrib "colspan" "2"]
                           [S ("Transaction " ++ hash tx ++ " at time "
                               ++ show (est2time (time tx)))]],
    thdrs (words "From To"),

-- now, check this out! A table of tables!

     tr (map E [
     tabulate [Attrib "border" "1"] [thdrs ["Address"]] (inputs tx),
     tabulate [Attrib "border" "1"] [thdrs (words "Address BTC")] (out tx)])])

-- and 2. A high-level summary view, when looking at all transactions of a block:

data XSummary = XSum Hash Hash (LocalTime, BitCoin)

transaction2Sum :: Hash -> Transaction -> XSummary
transaction2Sum blk =
   uncurry (XSum blk) . (hash &&& (est2time . time &&& val2BTC . sum . map value . out))

tradeLink :: XSummary -> Element
tradeLink (XSum blk trd _) = link' "trade" (blk ++ ('|':trd)) trd

instance Rasa XSummary where
   printRow x@(XSum _ _ (tim, btc)) =
      tr [E $ tradeLink x, S $ show tim, S $ show btc]

-- BLOCK -----------------------------------------------------------------

-- So, to show an entire block with all its transactions:

block2HTML :: Block -> Element
block2HTML blk = tabulate [Attrib "border" "1"]
   [Elt "tr" [Attrib "colspan" "3", Attrib "bgcolor" "CCFFCC"]
       [E $ Elt "th" [] [S ("Transactions for Block " ++ hash blk)]],
    thdrs (words "Hash Time BTC")]
   (map (transaction2Sum (hash blk)) (tx blk))

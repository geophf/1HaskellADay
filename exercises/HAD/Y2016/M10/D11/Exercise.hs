module Y2016.M10.D11.Exercise where

-- import Snap.Core -- if you want to use the Snap Framwork

-- Below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.BlockInfo
import Data.BlockChain.Block.Types
import Data.BlockChain.Block.Utils
import Data.XHTML

{--
Okay. So, yesterday we got the blocks process on the current day. Great! ...
and as a haskell data value, too. Great!

But, ugh. I don't want to troll through 88 values by hash and time as an
int, and I don't want to do that in some Haskell-REPL, either.

Today's problem:

Write a webservice that outputs the blocks from the block chain we got yesterday
as an HTML table of hash-id and time-in-human-readable-format. So, when you go
to your web-service URL http://.../blocks you get something like

Blocks for <date>

Block Id 1    |     Date1
Block Id 2    |     Date2

on a webpage.
--}

blocks2HTML :: Blocks -> Element
blocks2HTML = undefined

main :: IO ()
main = undefined

{-- BONUS -----------------------------------------------------------------

Using the work you've done before, e.g. Y2016.M10.D06.Exercise, make the 
block hash ID a link to show the transactions for that block in a pretty
HTML table format

--}

transactionsFromBlock :: Hash -> Element
transactionsFromBlock hashId = undefined

-- OR! simply make the REST call to your webservice from that exercise

-- either way.

-- Show your Haskell-powered webservice in all its Haskell-y glory.

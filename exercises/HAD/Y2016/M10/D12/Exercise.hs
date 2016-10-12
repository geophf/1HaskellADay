module Y2016.M10.D12.Exercise where

-- below import available from 1HaskellADay git repository

import Data.BlockChain.Block.BlockInfo
import Data.XHTML

{--
I've noticed that I've spent a lot of time, recently, encoding Haskell values
(lists of them) to HTML TR elements so as to show them on an HTML page in a
table, obviously.

It'd be helpful, then, if there were some generic way to transform a Haskell
value to a table row, and, keeping with that generality, have some metadata
to describe the class of values in each column.

Take, for example, yesterday's exercise. We have a data structure: Blocks
that has a list of rows with hash, height, and time and we want to show the 
hash and time. Wouldn't it be great if there were some generic HTML printer
for Haskell values like these?

Today's Haskell exercise.

declare a class, Rasa, that takes some Haskell value and returns an HTML TR
(table row) representing that value.
--}

class Rasa a where
   printRow :: a -> Element

{--
Great. Now that you have that, define a function tabluate that takes a list of
Attributes, a list of Elements that preceed the table body, and a list of Rasa
values and returns an HTML TABLE element that packages them all together.

So, if BlockInfo were a Rasa instance then

tabluate [Attrib "border" "1"] [table header for columns Hash and Time]
         [BlockInfo 13 "adkfjkjhfa" 1399482, BlockInfo 227 "adfjkhdf" 1400122]

would give an HTML table that would output the BlockInfo values in rows with
their data in the appropriately labeled columns.
--}

tabluate :: Rasa a => [Attribute] -> [Element] -> [a] -> Element
tabluate attribs headers rows = undefined

-- Download the blocks for today and show the block info as an HTML table

-- You do get the joke today, I hope.
-- tabluate :: Rasa
-- Right?

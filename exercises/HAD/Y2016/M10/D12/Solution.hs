module Y2016.M10.D12.Solution where

-- below import available from 1HaskellADay git repository

import Data.BlockChain.Block.BlockInfo
import Data.BlockChain.Block.Utils
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

instance Rasa BlockInfo where
   printRow (BlockInfo _ h t) =
      Elt "tr" [] (map (E . Elt "td" [] . pure)
                       [E (Elt "a" [Attrib "href" ("http://127.0.0.1:8080/block/" ++ h)] [S h]), S . show $ est2time t])

tabluate :: Rasa a => [Attribute] -> [Element] -> [a] -> Element
tabluate attribs headers rows =
   Elt "table" attribs (map E (headers ++ map printRow rows))

-- Download the blocks for today and show the block info as an HTML table

-- You do get the joke today, I hope.
-- tabluate :: Rasa
-- Right?

{--
*Y2016.M10.D12.Solution> readTodaysBlockInfo ~> blks
*Y2016.M10.D12.Solution> length $ blocks blks ~> 106
*Y2016.M10.D12.Solution> let table = tabluate [Attrib "border" "1"]
      [Elt "tr" [] (map (E . Elt "th" [] . pure . S) ["Hash ID", "Date"])]
      (blocks blks)
*Y2016.M10.D12.Solution> writeFile "Y2016/M10/D12/table.html" 
           (show (rep (Doc [Elt "title" [] [S "Today's Blocks"]] [table])))

Moving Rasa and tabulate to Data.XHTML
--}

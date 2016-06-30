{-# LANGUAGE TupleSections #-}

module Data.SymbolTable.Decompiler where

{-- A solution to the problem posted at http://lpaste.net/6108657252669849600
@1HaskellADay solution for 2015-03-15 -- YIKES! THE IDES OF MARCH!

So, yesterday, we ran into a bit of a problem in that when the stocks were
read in there were symbols newer than what Graph.Stocks supported. That's a
problem. It could be easily side-stepped by treating symbols as strings, but
then you lose the typeable properties that you have from Graph.Stocks.Stock.

Or, the new file could be scanned, parsing out the stock symbols, and a new
symbol-table created, allowing us to generate a new Graph.Stocks module, but
then what happens if the new set is a much smaller subset of stock symbols,
which is the case here.

So, we need to be able to retain what we already know and add to that knowledge
base.

We need a decompiler. AND we need to mantain the meta-information, like: that
the symbol-table is named, so that a new generation of, e.g.: Graph.Stocks
(or, now that I think of it: Analytics.Trading.Data.Symbols) goes smoothly.

We'll do this for today's Haskell problem.

Given an enumerable type that also has a show-representation, realize from that
a named symbol table
--}

import Control.Arrow
import Control.Monad.State

import Control.Scan.CSV (csv)
import qualified Data.BidirectionalMap as BDM
import Data.SymbolTable
import Data.SymbolTable.Compiler

decompile :: (Enum a, Show a) => DataTypeName -> a -> NamedSymbolTable
decompile typename =
   (typename,)
   . uncurry SymT
   . (BDM.fromList &&& maximum . map snd)
   . map (show &&& fromEnum)
   . enumFrom

type NamedSymbolTable = (DataTypeName, SymbolTable)

{--
*Data.SymbolTable.Decompile> let syms = decompile "Symbol" S0 ~>
("Stock",SymT {table = BDMap fromList [("AA",0),("AAPL",1),("ABB",2),...]})

Okay, great. Now using Graph.GapAnalysis.dequoteLikedetox, import the data at 

http://lpaste.net/raw/1146727925542813696

parse out the stock symbols, add them to the NamedSymbolTable.

Then save out the revised symbol table as module Analytics.Trading.Data.Stocks 
with the data type name Symbol. Use Data.SymbolTable.Compiler.compile to do this
--}

updateSyms :: NamedSymbolTable -> [String] -> NamedSymbolTable
updateSyms (name, symtable) = (name,) . (`execState` symtable) . mapM_ fromEnumS

{--
*Data.SymbolTable.Decompile> length . BDM.toList . table $ snd syms ~> 505
*Data.SymbolTable.Decompile> dequoteLikedetox "clusters/quoteyTop5sAll.csv" ~> newsyms
*Data.SymbolTable.Decompile> let topsyms = map (head . csv) newsyms 
*Data.SymbolTable.Decompile> take 4 topsyms ~> ["AA","AAPL","ABBV","ABEV"]
*Data.SymbolTable.Decompile> let latest = updateSyms syms topsyms 
*Data.SymbolTable.Decompile> length . BDM.toList . table $ snd latest ~> 520

YAY! for 15 new symbols!

*Data.SymbolTable.Decompile> compile "Symbol" "Analytics.Trading.Data.Stocks" (snd latest)

... resulting module saved at http://lpaste.net/1643064155973877760
--}

-- Tomorrow we'll look at creating a tool to read stocks and shows on the top 5s
-- directly from a REST endpoint ... using ... JSON! YAY!

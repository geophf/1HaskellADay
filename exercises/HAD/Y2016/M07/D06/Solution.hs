module Y2016.M07.D06.Solution where

import Control.Monad.State (execState)
import Data.Char (isAlpha, toUpper)
import qualified Data.Set as Set
import Data.SymbolTable
import Data.SymbolTable.Compiler

import Network.HTTP

{--
Pride and Prejudice.

http://www.gutenberg.org/cache/epub/1342/pg1342.txt

Extract ONLY the words from that file and load them all into a SymbolTable.

Note: 'wife.' is not a word but 'wife' (no period or 'full stop') IS.
--}

type Novel = String

prideAsSymbols :: FilePath -> IO SymbolTable
prideAsSymbols url =
   simpleHTTP (getRequest url) >>= getResponseBody >>= pure . wordsOnly
  
wordsOnly :: Novel -> SymbolTable
wordsOnly =  (`execState` empty) . mapM_ fromEnumS
   . Set.toList . Set.fromList . map regularize . words

regularize :: String -> String
regularize = map toUpper . filter isAlpha

{--
*Y2016.M07.D06.Solution> prideAsSymbols "http://www.gutenberg.org/cache/epub/1342/pg1342.txt" ~> syms
*Y2016.M07.D06.Solution> top syms ~> 7017
--}

{-- BONUS -----------------------------------------------------------------

Save out the symbol-table you've created as its own module.

Hint: Data.SymbolTable.Compiler may help here
--}

compilePnP :: DataTypeName -> FilePath -> SymbolTable -> IO ()
compilePnP = compile

{--
*Y2016.M07.D06.Solution> compile "ElizabethanEnglish" "Y2016.M07.D06.PrideAndPrejudice" syms
You can see the results saved in this directory

Or, let's take a look ourselves here:

geophf:HAD geophf$ ghci Y2016/M07/D06/PrideAndPrejudice.hs 
*Y2016.M07.D06.PrideAndPrejudice> S101 ~> ACTIVITY
*Y2016.M07.D06.PrideAndPrejudice> S7007 ~> YOUNGER

Sweet! We now have our own symbol-table of 7k+words/symbols!
--}

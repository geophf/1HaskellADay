import Data.SymbolTable
import Data.SymbolTable.Compiler

{--
So, looking at test.xml at Y2016/M06/D01/ you see I did something sneaky!

What did I do? Well, the original names and classnames are not for public 
consumption, so I loaded them into a symbol-table (the original names), then
replaced them with stock/security symbols from another symbolo table.

I just felt 'S0.S1.S2...' etc weren't very compelling substitutes.

Well, we're going to do the same thing in a piece-wise manner.

Piece 1 is today, and that is to load an encoding symbol tab le with a set of
symbols. What is our source? Let's use the most popular book on gutenberg press:
Pride and Prejudice.

http://www.gutenberg.org/cache/epub/1342/pg1342.txt

Extract ONLY the words from that file and load them all into a SymbolTable.

Note: 'wife.' is not a word but 'wife' (no period or 'full stop') IS.

So, with that caveat. HAVE AT IT!
--}

prideAsSymbols :: FilePath -> IO SymbolTable
prideAsSymbols = undefined

-- How many symbols are in the symbol table?

{-- BONUS -----------------------------------------------------------------

Save out the symbol-table you've created as its own module.

Hint: Data.SymbolTable.Compiler may help here
--}

compilePnP :: FilePath -> SymbolTable -> IO ()
compilePnP filenameOut symbols = undefined

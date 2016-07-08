module Y2016.M07.D07.Solution where

import Data.SymbolTable
import Data.SymbolTable.Decompiler

import Y2016.M07.D06.PrideAndPrejudice
import Y2016.M07.D06.Solution

{--
Pride and Prejudice and ... Dracula? Anyone? No?

So, now that we have Pride and Prejudice encoded (see above), let's use it
to encode another work.

Today's exercise.

Dracula, by Bram Stoker, is available on the web here:

http://www.gutenberg.org/cache/epub/345/pg345.txt

Encode it into a symbol table as you did for Pride and Prejudice. Good.
(there's no need to duplicate work already done: that's why we have access
to modules out there already.
--}

dracula2Syms :: Novel -> SymbolTable
dracula2Syms = wordsOnly

{--
*Y2016.M07.D07.Solution> fetchURL "http://www.gutenberg.org/cache/epub/345/pg345.txt" ~> drac
*Y2016.M07.D07.Solution> dracula2Syms drac ~> dracsyms
*Y2016.M07.D07.Solution> top dracsyms ~> 10724
--}

reinterpretDracula :: SymbolTable -> SymbolTable -> Novel -> Novel
reinterpretDracula pnpsyms draculasyms =
  unwords . map (reindex draculasyms pnpsyms . regularize) . words

reindex :: SymbolTable -> SymbolTable -> String -> String
reindex from to word = strVal to (intVal from word * top to `div` top from)

{--
*Y2016.M07.D07.Solution> let pnpsyms = snd (decompile "foo" S0)
*Y2016.M07.D07.Solution> let janesdrac = reinterpretDracula pnpsyms dracsyms drac
*Y2016.M07.D07.Solution> unwords . take 10 . drop 1500 $ words janesdrac
"THOSE SWELLED DESERTS WHATSOMETHING SLY FALSEHOOD BRILLIANCY PATRONESS NONE INCLUDING"

And there it is: Bram Stoker Dracula, reenvisioned by Jane Austen.
--}

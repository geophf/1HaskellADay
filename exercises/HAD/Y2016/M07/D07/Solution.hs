module Y2016.M07.D07.Solution where

import Control.Monad.State
import qualified Data.Set as Set
import Data.SymbolTable
import Data.SymbolTable.Decompiler
import Network.HTTP

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

type Novel = String

dracula2Syms :: Novel -> SymbolTable
dracula2Syms = (`execState` empty) . mapM_ fromEnumS . Set.toList . Set.fromList
   . map regularize . words

{--
(remember that "(Illustration:" is not a word but "Illustration" is.)

Now, re assemble the document Dracula, but substituting each word in that
novel for the a word similarly indexed from the Pride and Prejudice novel.

What we are doing is encoding Dracula in English but using a very different
mode of expression to encode it. What is the resulting novel you produce?
--}

reinterpretDracula :: SymbolTable -> SymbolTable -> Novel -> Novel
reinterpretDracula pnpsyms draculasyms drac = undefined

{--
Hint: lookup the symbol index for each word in the novel Dracula, then, using
that symbol index, replace it with the word in the Pride and Prejudice symbol
table.

Hint: You can convert a Haskle module of a symbol-table to SymbolTable
by using a function in Data.SymbolTable.Decompiler.

Hint: You can use Network.HTTP to interact with these novels on project
gutenberg.
--}


module Y2016.M07.D12.Exercise where

import Data.SymbolTable
import Data.SymbolTable.Compiler

import RID.Analysis
import RID.Report
import RID.Tree

{--
Okay, yesterday we, well: you looked at ingesting the RID from JSON.

I didn't because I was having a heart attack. Fun times. Fun times.

TODAY, whilst I have an angiogram leading to a stent or surgery, YOU will
rerealize the internal representation of the RID into a compiled form.

This is very much along the lines of the SymbolTable compiler (which is 
included in the import as a reference).

For today's Haskell problem, now that we have internalized the RID from JSON,
let's externalize it as a Haskell module.
--}

compileRID :: FilePath -> RID -> IO ()
compileRID modulename rid = undefined

{--
The function compileRID takes an output file name, the RID, and outputs a 
Haskell module of the RID, so that when loaded gives you the fully-realized
RID that you can enRIDify documents...

Is 'enRIDify' a word?
Yes.

Thoughts:

Plusses of compiling JSON: you don't have to load it, you don't have dependency
on an exernal property file
Minusses of compiling JSON: lose dynamicism (that you don't want, anyway), can
not share across languages.
--}

{--
When you compile the RID, load up your new module and analyze Pride and Prejudice
on gutenberg press:

http://www.gutenberg.org/cache/epub/1342/pg1342.txt

What are the top ten cognitive influences in that work?
--}

analyzePnP :: FilePath -> IO RIDAnalysis
analyzePnP url = undefined

-- Hint: you may wish to look at the analysis as a report, too.

module Y2016.M07.D08.Solution where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Set as Set
import Control.Scan.CSV
import Data.SymbolTable
import Data.SymbolTable.Decompiler
import Text.HTML.TagSoup

import Y2016.M07.D06.PrideAndPrejudice
import Y2016.M07.D07.Solution (reindex)

-- convert every name and classname to an encoded value and then save out the
-- result as the same XML structure.

rerealizeXMLasXML :: FilePath -> SymbolTable -> IO ()
rerealizeXMLasXML file tosyms = readFile file >>= \junits ->
   let tags = parseTags junits
       fromsyms = createSyms tags
       newtags = map (changeNames tosyms fromsyms) tags
   in  putStrLn (renderTags newtags)

changeNames :: SymbolTable -> SymbolTable -> Tag String -> Tag String
changeNames to from t =
   (if isTagOpen t then substNames from to else id) t

-- first we need to extract all names (name, classname, project) and load them
-- into a symbol table:

-- do this for test.xml in this directory

{-- Okay the structure is

testrun name project tests started failures errors ignored
 testsuite (name) time
  testcase (name) (classname) time
  testcase ...
 testsuite (name) time
  testcase ...
  ...

where testsuite name is dot.qualified.name and so is classname

So, actually, a better way to go about this is to rend all the qnames, put them
into a set and then symbol-table-ize those values, instead of querying the
symbol table at each qualifying name.
--}

createSyms :: [Tag String] -> SymbolTable
createSyms tags =
   let opentags = filter isTagOpen tags -- (parseTags html)
       qnames = [fromAttrib] <*> replacers <*> opentags
       qualifers = Set.fromList (concatMap (rend '.') qnames)
   in  execState (mapM_ fromEnumS (Set.toList qualifers)) empty

{--
lookupNames :: Tag String -> [String]
lookupNames = ([fromAttrib] <*> ["name","classname","project"] <*>) . pure

Okay, we have the tags, we have the symbols, now we need to one-for-one replace
the qnames with some encoding
--}

replacers :: [String]
replacers = ["name","classname","project"]

type ReplaceF a = SymbolTable -> SymbolTable -> a -> a
type Attr = Attribute String

substNames :: ReplaceF (Tag String)
substNames syms f (TagOpen n attrs) =
   TagOpen n (map (replaceAttrib syms f) attrs)

-- for each of the qname-attributes we need to rend the name and replace it 
-- with the new name

replaceAttrib :: ReplaceF Attr
replaceAttrib syms f (x,y) =
   (x, (if   x `elem` replacers
        then intercalate "." . map (reindex syms f) . rend '.'
        else id) y)

{--
*Y2016.M07.D08.Solution> let pnpsyms = snd $ decompile "syms" S0
*Y2016.M07.D08.Solution> rerealizeXMLasXML "Y2016/M07/D01/test.xml" pnpsyms 
... re-realized xml is saved here as rerealized.xml

... I like that COFFEE is the root qualified name. This pleases me much.
--}

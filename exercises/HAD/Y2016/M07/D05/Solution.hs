module Y2016.M07.D05.Solution where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Set as Set
import Control.Scan.CSV
import Data.SymbolTable
import Data.SymbolTable.Decompiler
import Text.HTML.TagSoup

-- convert every name and classname to an encoded value and then save out the
-- result as the same XML structure.

encodeXMLnames :: FilePath -> IO ()
encodeXMLnames file = readFile file >>= \junits ->
   let tags = parseTags junits
       fromsyms = createSyms tags
       newtags = map (changeNames fromsyms) tags
   in  putStrLn (renderTags newtags)

changeNames :: SymbolTable -> Tag String -> Tag String
changeNames from t =
   (if isTagOpen t then substNames from (('S':) . show) else id) t

-- first we need to extract all names (name, classname, project) and load them
-- into a symbol table:

{--
   let syms = execState (mapM_ encodeQname . concat . ([fromAttrib] <*> ["name", "classname", "project"] <*>) . filter isTagOpen $ parseTags html) empty
   in  pure syms
--}

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

type ReplaceF a = SymbolTable -> (Int -> String) -> a -> a
type Attr = Attribute String

substNames :: ReplaceF (Tag String)
substNames syms f (TagOpen n attrs) =
   TagOpen n (map (replaceAttrib syms f) attrs)

-- for each of the qname-attributes we need to rend the name and replace it 
-- with the new name

replaceAttrib :: ReplaceF Attr
replaceAttrib syms f (x,y) =
   (x, (if   x `elem` replacers
        then intercalate "." . map (f . intVal syms) . rend '.' else id) y)

{--
*Y2016.M07.D05.Solution> encodeXMLnames "Y2016/M07/D01/test.xml"
<?xml version="1.0" encoding="UTF-8" ?>
<testrun name="S9.S6" project="S0" tests="22" started="22" failures="0" errors="0" ignored="0">
  <testsuite name="S5.S7.S4.S32.S8.S1" time="0.0">
    <testcase name="S24" classname="S5.S7.S4.S32.S8.S1" time="0.0"></testcase>
    <testcase name="S23.S33" classname="S5.S7.S4.S32.S8.S1" time="0.0"></testcase>

...
--}

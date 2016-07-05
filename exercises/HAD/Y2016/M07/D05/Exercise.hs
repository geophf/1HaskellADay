module Y2016.M07.D05.Exercise where

{--
So, let's say, for example, hypothetically speaking, that we have a set of
jUnit test cases as XML, or some other sensitive data that you wish to obscure
before you share with a larger public audience?

How do you translates "sensitive data" to 'coded data'?
--}

import Control.Monad.State
import Text.HTML.TagSoup
import Data.SymbolTable

-- convert every name and classname to an encoded value and then save out the
-- result as the same XML structure.

encodeXMLnames :: FilePath -> IO ()
encodeXMLnames = undefined

-- do this for test.xml in the Y2016/M07/D01/ directory

-- hint: SymbolTable takes a string and returns an enumerated value for it
-- (an Int), a possible encoding, then, for "foo" could be "S1", for "bar"
-- could be "S2" ... etc.

-- We'll look at more clever encodings throughout this week.

{-- BONUS -----------------------------------------------------------------

You see in test.xml that the qualified names follow this pattern

a.b.c.d.e...

Instead of encoding the entire qualified name, encode each of the qualifying
names of a qualified name. Do you have more or less symbols that way? Is that
encoding more or less sensical?
--}

encodeQname :: String -> State SymbolTable ()
encodeQname qualifiedName = undefined

-- so "a.b.c.d.e" adds or verifies 5 symbols to the SymbolTable

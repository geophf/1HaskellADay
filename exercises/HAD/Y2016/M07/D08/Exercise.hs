module Y2016.M07.D08.Exercise where

{--
Where two streams meet, you fish.

Now that we can transpose one body of work to some encoding (that we can later
decode) (exercise for the reader: prove that to yourself).

And now that we can capture the qualifying names of a qualified name.

Let's do both.

From Y2016/M07/D01/test.xml, convert all the qualified names of the name,
classname and project attributes to symbols in the PrideAndPrejudic type
--}

import Control.Monad.State
import Text.HTML.TagSoup
import Data.SymbolTable
import Y2016.M07.D06.PrideAndPrejudice

rerealizeXMLasXML :: FilePath -> IO ()
rerealizeXMLasXML = undefined

-- so the name AYA.BCPC.ASNA.MPC.BLOCK.ACT-A becomes something like:
-- FACT.UNIVERSALLY.ACKNOWLEDGED.THAT.A.SINGLE.MAN

-- or something like that

-- Hint: Y2016.M07.D01 looked at parsing the XML
-- Hint: Y2016.M07.D07 looked at replacing symbols with Pride and Prejudice
-- symbols

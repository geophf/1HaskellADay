module Y2017.M09.D27.Exercise where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL

-- below import available via 1HaskellADay git repository

import Y2017.M09.D22.Exercise (dir, arts)      
import Y2017.M09.D25.Solution (parseKV)
import Y2017.M09.D26.Exercise (extractArticles)

{--
What's in a name?

So, (day before) yesterday, you were able to break down a block of text into a
set of attributed values that make up an article. One of those attributed values
was author. Another possible attributed value (in the metadata) is "People":

>>> (art3:articles) <- extractArticles <$> BL.readFile (dir ++ arts)
>>> author art3
"Nagourney, Adam"
>>> Map.lookup "People" (metadata art3)
Just "Cuomo, Mario M"

But here's the thing. A person's name has structure in the data store:
--}

data Person = Name { source :: String, family, given, rest :: Maybe String }
   deriving (Eq, Ord, Show)

-- parse the author and "People" metadata to Person values

parsePerson :: String -> [Person]
parsePerson txt = undefined

-- note the return type. Why is it a list? Because of this:

multis :: String
multis = "People: Putin, Vladimir Batiashvili, Lisa Gergiev, Valery"

-- Look at the structure of these names and as compared to Cuomo's name or
-- Nagourney's name. Hm. Parse out the above and have the parser work for
-- Cuomo, multis and Nagourney.

-- hint: you may wish to use parseKV from the Y2017.M09.D25 import to help
-- separate the "People:"-key from the names in the multis function.

-- Also, this one, because why? Because real data is unpredictable:

bushes :: String
bushes = "People: Bush, George H W Bush, Jeb Bush, Columba"

-- and how about this?

middleName :: String
middleName = "People: Paul, Rand Christie, Christopher J Huckabee, Mike"

-- BONUS -----------------------------------------------------------------

thai :: String
thai = "People: Yingluck Shinawatra"

{-- HOLD THE PHONE ON THAT! THERE'S NO COMMA AND YINGLUCK IS THE FIRST NAME?!?
IS YINGLUCK A FAMILY NAME OR THE GIVEN NAME?!?

Research this, find out, then build the appropriate parser that gives the
correct result for Bush, George H W and thai and multis and author.

Sheesh! Why does production data have to be so difficult?!?

and there's two more examples from the data extract:

Reagan, Ronald Wilson
Francis (Pope)

DOES EVERY SINGLE NAME FOR EACH ARTICLE HAVE TO HAVE A DIFFERENT FORMAT? Sheesh
--}

module Y2017.M09.D27.Solution where

import Control.Monad
import Data.Char (isAlpha)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (maybeToList)

-- below import available via 1HaskellADay git repository

import Control.DList
import Control.Logic.Frege ((-|))

import Y2017.M09.D22.Solution (dir, arts)      
import Y2017.M09.D25.Solution (parseKV)
import Y2017.M09.D26.Solution (extractArticles)

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
parsePerson = parsePerson' [] . words

parsePerson' :: [Person] -> [String] -> [Person]
parsePerson' ans [] = ans
parsePerson' acc (n:ames) =
   let (p,rest) = parseEachName n ames in parsePerson' (p:acc) rest

{--
So, now we need to compute what parseEachPerson is for each form of person.

Basically the name parsing devolves to two forms:

lastname_comma OR anything else.

If we're in the lastname_comma-mode, we stay in that mode, and names are
from last name to last name. But if we're in another mode, then it's
first name and whatever else.

Oh, and all of these values are optional. Okay.
--}

parseEachName :: String -> [String] -> (Person, [String])
parseEachName n = (if isLastName n then lastNameParsing else firstNameParsing) n

isLastName :: String -> Bool
isLastName = (== ',') . last

lastNameParsing, firstNameParsing :: String -> [String] -> (Person, [String])

-- okay: we know the last name:

lastNameParsing ln (fn:rest) =
   let (names, trail) = grabnames rest emptyDL in
   (Name (unwords (ln:fn:maybeToList names)) (Just (init ln)) (Just fn) names, trail)

-- here we know the first name:

firstNameParsing fn rest =
   (uncurry (flip (Name (unwords (fn:rest))) (Just fn)) $ parseFirstName rest,
    [])

-- here (in firstname-context) we determine if the rest also has a last name:

parseFirstName :: [String] -> (Maybe String, Maybe String)
parseFirstName [] = (Nothing, Nothing)
parseFirstName (r:est) | isAlpha (head r) = (Just r, list2name est)
                       | otherwise        = (Nothing, list2name (r:est))

-- here (in lastname-context) we get the first name and rest of the name:

grabnames :: [String] -> DList String -> (Maybe String, [String])
grabnames [] dl = (list2name (dlToList dl), [])
grabnames stream@(h:t) dl | isLastName h = (list2name (dlToList dl), stream)
                          | otherwise    = grabnames t (dl <| h)

list2name :: [String] -> Maybe String
list2name ans = not (null ans) -| Just (unwords ans)

-- note the return type. Why is it a list? Because of this:

multis :: String
multis = "People: Putin, Vladimir Batiashvili, Lisa Gergiev, Valery"

{--
>>> parseKV (BL.pack multis)
("People","Putin, Vladimir Batiashvili, Lisa Gergiev, Valery")
>>> parsePerson (BL.unpack (snd it))
[Name {source = "Gergiev, Valery", family = Just "Gergiev", given = Just "Valery", rest = Nothing},
 Name {source = "Batiashvili, Lisa", family = Just "Batiashvili", given = Just "Lisa", rest = Nothing},
 Name {source = "Putin, Vladimir", family = Just "Putin", given = Just "Vladimir", rest = Nothing}]
--}

-- Look at the structure of these names and as compared to Cuomo's name or
-- Nagourney's name. Hm. Parse out the above and have the parser work for
-- Cuomo, multis and Nagourney.

-- hint: you may wish to use parseKV from the Y2017.M09.D25 import to help
-- separate the "People:"-key from the names in the multis function.

-- Also, this one, because why? Because real data is unpredictable:

bushes :: String
bushes = "People: Bush, George H W Bush, Jeb Bush, Columba"

{--
>>> parseKV (BL.pack bushes)
("People","Bush, George H W Bush, Jeb Bush, Columba")
>>> parsePerson (BL.unpack (snd it))
[Name {source = "Bush, Columba", family = Just "Bush", given = Just "Columba", rest = Nothing},
 Name {source = "Bush, Jeb", family = Just "Bush", given = Just "Jeb", rest = Nothing},
 Name {source = "Bush, George H W", family = Just "Bush", given = Just "George", rest = Just "H W"}]
--}

-- and how about this?

middleName :: String
middleName = "People: Paul, Rand Christie, Christopher J Huckabee, Mike"

{--
>>> parseKV (BL.pack middleName)
("People","Paul, Rand Christie, Christopher J Huckabee, Mike")
>>> parsePerson (BL.unpack (snd it))
[Name {source = "Huckabee, Mike", family = Just "Huckabee", given = Just "Mike", rest = Nothing},
 Name {source = "Christie, Christopher J", family = Just "Christie", given = Just "Christopher", rest = Just "J"},
 Name {source = "Paul, Rand", family = Just "Paul", given = Just "Rand", rest = Nothing}]
--}

-- BONUS -----------------------------------------------------------------

thai :: String
thai = "People: Yingluck Shinawatra"

{-- HOLD THE PHONE ON THAT! THERE'S NO COMMA AND YINGLUCK IS THE FIRST NAME?!?
IS YINGLUCK A FAMILY NAME OR THE GIVEN NAME?!?

Research this, find out, then build the appropriate parser that gives the
correct result for Bush, George H W and thai and multis and author.

Sheesh! Why does production data have to be so difficult?!?

>>> parseKV (BL.pack thai)
("People","Yingluck Shinawatra")
>>> parsePerson (BL.unpack (snd it))
[Name {source = "Yingluck Shinawatra", family = Just "Shinawatra", given = Just "Yingluck", rest = Nothing}]

and there's two more examples from the data extract:

Reagan, Ronald Wilson

>>> parsePerson "Reagan, Ronald Wilson"
[Name {source = "Reagan, Ronald Wilson", family = Just "Reagan", given = Just "Ronald", rest = Just "Wilson"}]

Francis (Pope)

>>> parsePerson "Francis (Pope)"
[Name {source = "Francis (Pope)", family = Nothing, given = Just "Francis", rest = Just "(Pope)"}]

DOES EVERY SINGLE NAME FOR EACH ARTICLE HAVE TO HAVE A DIFFERENT FORMAT? Sheesh
--}

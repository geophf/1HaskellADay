module Y2017.M10.D30.Solution where

{--
Hello! Welcome back! Happy Monday!

Of course, when you're working in a start-up, the days can run together in a
blur, I've found.

So, we've been working with the NYT archive for more than a month now. Did you
notice anything about the data? I have:
--}

import Control.Arrow ((&&&))
import Control.Monad
import Data.Char (ord)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Control.DList

import Y2017.M10.D23.Solution -- for Article structure
import Y2017.M10.D24.Solution (articlesFromFile)

sampleText :: String
sampleText = "GALVESTON, Tex. \226\128\148 Adolfo Guerra, a landscaper"

-- The thing is, if you look at this in a text editor (worth its salt) you see:

-- GALVESTON, Tex. — Adolfo Guerra, a landscaper

{-- 
So, we need:

1. to identify the documents that have special characters
2. the context of where these special characters occur
3. once marked, replace the special characters with a simple ASCII equivalent

Let's do it.
--}

data Context = Ctx { ctxid :: Integer, spcCharCtx :: SpecialCharacterContext }
   deriving (Eq, Show)

type SpecialCharacterContext = Map SpecialChars [Line]
type SpecialChars = String
type Line = String

identify :: MonadPlus m => Article -> m Context
identify art = case extractSpecialChars (words $ fullText art) of
   []        -> mzero
   yup@(_:_) -> return (Ctx (artIdx art) (enmapify yup))

enmapify :: [(SpecialChars, Line)] -> SpecialCharacterContext
enmapify = Map.fromList . map (fst . head &&& map snd)
         . groupBy ((==) `on` fst) . sortOn fst

-- identifies the places in the text where there are special characters and
-- shows these characters in context ... now, since the full text is one 
-- continuous line, getting a line is rather fun, isn't it? How would you
-- form the words around the special characters? Also, how do you extract
-- the special characters from the full text body?

{--
>>> (identify (head hurr)) :: Maybe Context
Just (Ctx {ctxid = 321,
           spcCharCtx = fromList
              [("\226\128\148",["Rights movement \226\128\148 but it",
                                "your skin \226\128\148 those images",
                                "GALVESTON, Tex. \226\128\148 Adolfo Guerra,"]),
               ("\226\128\152",["to myself, \226\128\152Maybe this is"]),...]})
--}

type BodyContext = [String]

extractSpecialChars :: BodyContext -> [(SpecialChars, Line)]
extractSpecialChars [] = []
extractSpecialChars (w:ords) = esc [] w ords []
 
-- sees if we're at a set of special characters then accumulates a context
-- around those characters. N.b.: special characters can occur as their own
-- word, or in a word at the beginning, or within, or at the end of a word,
-- e.g. ("Don't..." he said) if the quotes and apostrophe and ellipse are 
-- special characters then you have an example of special characters all around
-- the word 'don't' including the (') within the word.

{--
>>> extractSpecialChars (words sampleText)
[("\226\128\148","GALVESTON, Tex. \226\128\148 Adolfo Guerra,")]
--}

esc :: [String] -> String -> [String] -> [(SpecialChars, Line)] -> [(SpecialChars, Line)]
esc ctx lastword [] ans = 
   case specialChars lastword [] of
      [] -> ans
      spc@(_:_) -> zip spc (repeat (unwords . reverse $ take 5 ctx)) ++ ans
esc ctx wrd (h:t) acc =
   esc (wrd:ctx) h t
   ((case specialChars wrd [] of
      [] -> []
      spc@(_:_) ->
         zip spc (repeat (unwords (reverse (take 2 ctx) ++ (wrd:h:take 1 t))))) ++ acc)

-- seeks to special character set
-- (n.b.: we must account for multiple special characters in one word.)

specialChars :: String -> [String] -> [String]
specialChars [] ans = ans
specialChars (c:hars) acc | ord c > 127 =
    let (spec,rest) = charing hars (dl' c) in
    specialChars rest (spec:acc)
                          | otherwise   = specialChars hars acc

-- now that we're in special character territory, accums them until done

charing :: String -> DList Char -> (String, String)
charing [] acc = (dlToList acc, [])
charing (c:hars) acc | ord c > 127 = charing hars (acc <| c)
                     | otherwise   = (dlToList acc, c:hars)

-- intentionally dropped c: we know it's not a special character, so drop it.

-- With the set of articles from Y2017/M10/D24/hurricanes.json.gz
-- What are the special characters, in which articles, and in what contexts?

{--
>>> hurr <- articlesFromFile "Y2017/M10/D24/hurricanes.json.gz" 
>>> ans = hurr >>= identify
>>> length ans
169
>>> take 5 (map ctxid ans)
[321,655,1211,2184,3171]
>>> Map.keys . spcCharCtx $ head ans
["\226\128\148","\226\128\152","\226\128\153","\226\128\153\226\128\157",
 "\226\128\156","\226\128\157"]
--}

-- Tomorrow we'll look at building a replacement dictionary

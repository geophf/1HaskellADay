{-# LANGUAGE TupleSections, TemplateHaskell #-}

module RID.Tree where

{-- A solution to the problem posted at http://lpaste.net/4829042411923570688

Today's Haskell problem involves the free on-line RID ('regressive imagery
dictionary'):

http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/regressive-imagery-dictionary-by-colin-martindale-free/

(choose your language of choice)

We're working with just the English primary cognition today, which I've 
captured here:

http://lpaste.net/raw/8106042088711258112

But you can used the primary categories from your language of choice, should 
you prefer. Let's say your language of choice is not represented: then write 
your language's RID and submit it to the site. Homework.

Now that you've got that, let's examine the structure.

Did that? Great.

Now represent that structure as some Haskell form.
--}

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Graph
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Network.HTTP

data Category = Cat { name :: String, cats :: Map String Category }
              | WordSet { forCat :: String, roots :: Map WordFragment RootWord }
   deriving (Eq, Show)

nameof :: Category -> String
nameof (WordSet nm _) = nm
nameof (Cat nm _)     = nm

-- or just: nameof = view _1
-- but yeah.

-- Okay, firstly we need parse out the data from the structure ...

urlToTxt :: FilePath -> IO String
urlToTxt = simpleHTTP . getRequest >=> getResponseBody

{-- a solution to the problem posted at http://lpaste.net/7116276040808267776

Okay, LAST week we looked at cataloguing the raw RID/Regressive Imagery 
Dictionary-data into some structure or another.

That's all well and good.

But what are these data good for? Don't we wish to use them to some end?

We'll get to that this week.

But first! The RID is more than just primary cognition, it's also secondary
cognition and emotions, so let's expand the structure ... structurally ... to
include those roots and categories.
--}

data Kinds = -- eheh 'datakinds' hehe
             Root | Cog Cognition | Catg String | Rwrd RootWord
   deriving (Eq, Ord, Show)

-- Kinds is used both by RID.Graph and RID.Relation

data Cognition = PRIMARY | SECONDARY | EMOTIONS
   deriving (Eq, Ord, Enum, Show, Read)

data RID = Cognate { cogs :: Map Cognition (Map String Category) } 
   deriving (Eq, Show)

-- Yes, the RID is a Map of Maps of Categories (which are Maps, btw)

mk :: FilePath -> String -> FilePath
mk dir file = dir ++ ('/':file ++ ".CAT")

parser :: Cognition -> ParsingCatS
parser PRIMARY = parseCategory
parser _       = parseWordSet

parseBranch :: Cognition -> RawCats
          -> ((Cognition, Map String Category), RawCats)
parseBranch cog = first (cog,) . readIn (parser cog)

-- so:

readBranch :: Cognition -> FilePath -> IO (Cognition, Map String Category)
readBranch cog = liftM (fst . parseBranch cog . marshall . lines) . readFile

-- and with readBranch we can define our readinRID as a monadic sequence:

readinRID :: FilePath -> IO RID -- assuming we put everything in one place...
readinRID dir =
   readBranch PRIMARY (mk dir "prim") >>= \prims ->
   readBranch SECONDARY (mk dir "snd")  >>= \snds  ->
   readBranch EMOTIONS (mk dir "emot") >>= \emot  ->
   return (Cognate (Map.fromList [prims, snds, emot]))

{-- AHAHAHAHA! The first response:
*Main> readinRID "RID" ~> Cognate {(PRIMARY,{}), (SECONDARY,{}), (EMOTIONS,{}}

... well, that is one instance of a dictionary, but ... :/

So, after the fixes (commenting out the (1,_) stop) we have:

*Main> readinRID "RID" ~> rid
*Main> let (Cognate cog) = rid
*Main> length cog ~> 3
--}

readIn :: ParsingCatS -> RawCats -> (Map String Category, RawCats)

-- we need the ParsingCatS fn to tell us when it's done. The trick here is that
-- we know when tier == 1, we're done, so we use this specific structure to
-- guide our unfold. Works in this case, but perhaps not generally. FWIW.

-- BREAKING! 'Caveat emptor' is now pronounced 'FWIW'! ... FWIW.

readIn _ [] =  (Map.empty, [])
-- readIn _ stream@([(1, _)]:_) = (Map.empty, stream)
readIn fn lst =
   let (cat, rest) = fn lst
   in  first (Map.insert (nameof cat) cat) $ readIn fn rest

{--
As before, the raw primary cognition is at 
http://lpaste.net/raw/8106042088711258112

Secondary: http://lpaste.net/raw/4930045693239754752

Emotion: http://lpaste.net/raw/1484040414859100160

Read in the RID from these three sources. Note that the structure is different
for the primary and the other sources.

Answer the following questions:
--}

-- 1. How many categories are there? Don't include sub-categories.
--    What are they? Print out only the category names.

categories :: RID -> [Category]
categories = concatMap Map.elems . Map.elems . cogs

{--
*Main> map nameof (categories cog) ~>
["DEFENSIVE_SYMBOL","ICARIAN_IM","NEED","REGR_KNOL","SENSATION",
 "ABSTRACT_TOUGHT","INSTRU_BEHAVIOR","MORAL_IMPERATIVE","ORDER","RESTRAINT",
 "SOCIAL_BEHAVIOR","TEMPORAL_REPERE","AFFECTION","AGGRESSION","ANXIETY",
 "EXPRESSIVE_BEH","GLORY","POSITIVE_AFFECT","SADNESS"]
*Main> length it ~> 19
--}

marshall :: [String] -> RawCats
marshall = groupBy cmpFst . tail . map (flip wrdTier 0) 

type Tier = Int

wrdTier :: String -> Tier -> (Tier, String)
wrdTier ('\t':rest) t = wrdTier rest (succ t)
wrdTier wrd t = (t, chomp wrd)

chomp :: String -> String
chomp wrd = (if last wrd == '\r' then init else id) wrd

cmpFst :: Eq a => (a, b) -> (a, b) -> Bool
cmpFst = curry (uncurry (==) . join (***) fst)

{--
*Main Control.Monad> liftM lines $ urlToTxt "http://lpaste.net/raw/8106042088711258112"
*Main Control.Monad> let tiers = tail $ map (flip wrdTier 0) it ~>
[(1,"NEED"),(2,"ORALITY"),(3,"ABSINTH* (1)"),(3,"ALE (1)"),...]
*Main> let rawcats = groupBy cmpFst tiers ~>
[[(1,"NEED")],[(2,"ORALITY")],[(3,"ABSINTH* (1)"),(3,"ALE (1)"),..]

So, categories are simply a grouping of groups with their subgroups.
--}

type RawCats = [[(Tier, String)]]
type ParsingCatS = RawCats -> (Category, RawCats)

parseCategory :: ParsingCatS
parseCategory ([(t, cat)]:tail) =
   parseSubCategory (Cat cat Map.empty) (succ t) tail

 -- >> then parse the next subcategory

parseCategories :: RawCats -> [Category]
parseCategories [] = []
parseCategories rawcats =
   let (cat, rest) = parseCategory rawcats
   in  cat : parseCategories rest           -- NOT A FOLD! (why do I say this?)

{-- 
*Main> let (Cat cat stuff, rest) = parseCategory rawcats
*Main> length rest ~> 56
*Main> cat ~> "NEED"
*Main> Map.keys stuff ~> ["ANALITY","ORALITY","SEX"]

Cat "NEED" {("ANALITY",WordSet "ANALITY" {("ANAL",ANAL (1)),...}),
            ("ORALITY",WordSet "ORALITY" {("ABSINTH",ABSINTH* (1)),...}),
            ("SEX",WordSet "SEX" {("ADULTERER",ADULTERER* (1)),...})}
--}

parseSubCategory :: Category -> Tier -> ParsingCatS
parseSubCategory ans _ [] = (ans, [])
parseSubCategory c@(Cat cat m) tier all@([(t, catname)]:rest) =
   if t < tier then (c, all)
   else let (subgroup, r) =
                   parseSubCategory (Cat catname Map.empty) (succ t) rest
        in  parseSubCategory (Cat cat (Map.insert catname subgroup m)) t r
parseSubCategory (Cat cat m) t list@(grp:rest) = parseWordSet ([(t,cat)]:list)

parseWordSet :: ParsingCatS
parseWordSet ([(t,cat)]:words:rest) =
   (WordSet cat
            (Map.fromList $ map ((fst &&& uncurry RW) . realize . snd) words),
    rest)

realize :: String -> (String, Bool)
realize = second ((== '*') . head) . span isAlpha

-- Now answer the following questions.

type Path = [String]

-- So a Path for the root of the word ARTICHOK (represented as 'ARTICHOK* (1)')
-- has a representation something like PRIMARY -> NEED -> ORALITY -> ARTICHOK* (1)

-- So Path includes the word sought

type WordFragment = String

data RootWord = RW { root :: WordFragment, wildcard :: Bool }

instance Eq RootWord where
   RW w1 s1 == RW w2 s2 =
         w1 == w2                  -- first case
      || w1 `isPrefixOf` w2 && s1  -- second case
      || w2 `isPrefixOf` w1 && s2  -- its converse
   rw1     /= rw2 = not (rw1 == rw2) -- yeah, that. -- this should be default.

instance Ord RootWord where
   compare rw1@(RW w1 _) rw2@(RW w2 _) =
      if rw1 == rw2 then EQ else compare w1 w2

instance Show RootWord where
   show (RW wrd star) = wrd ++ (if star then "*" else "") ++ " (1)"

liftRW :: String -> RootWord
liftRW = flip RW False

-- *Main> RW "ABSINTH" True ~> "ABSINTH* (1)"

-- And now, to purify the document and the words therein by: 
-- 1. capitalizing all letters of a work
-- 2. removing all non-letters

readyWord :: String -> String
readyWord = map toUpper . filter isAlpha

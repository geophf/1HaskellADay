{-# LANGUAGE OverloadedStrings #-}

module Y2017.M05.D24.Solution where

import Data.Aeson   -- available via cabal
import Data.Aeson.Types (Parser)
import Data.Text (Text)

-- below imports available via 1HaskellADay git repository

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

{--
So, how far in depth do you go before you're 'done.'

There is a scientific approach to this if one has a tree or DAG with only
one root. Do we have this here?

Languages have roots, but language families have roots, too, and the families
of languages are in no way flat nor well-balanced.

Post this query to the wikidata endpoint and examine your results:
--}

languageRootsQuery :: String
languageRootsQuery = 
   unlines ["SELECT ?language ?languageLabel ?root ?rootLabel",
      "?parent ?parentLabel ?grand ?grandLabel ?great ?greatLabel ",
      "?great2 ?great2Label ?nspeakers WHERE {",
  "?language wdt:P31 wd:Q34770  .",
  "?language wdt:P279 ?root.",
  "OPTIONAL { ?root wdt:P279 ?parent.",
             "?parent wdt:P279 ?grand.",
             "?grand  wdt:P279 ?great.",
             "?great  wdt:P279 ?great2 }.",
  "?language wdt:P1098 ?nspeakers.",
   "SERVICE wikibase:label {",
     "bd:serviceParam wikibase:language \"en\" .",
   "}",
   "}  ORDER BY ?nspeakers"]

-- The structure of this query is as follows:

data Language = Lang { lang, root :: String, nspeakers :: Integer,
                       parent, grand, great, great2 :: Maybe String }
   deriving (Eq, Show)

{--
-- first of all we need to parse JSON to a Maybe-value. Aeson has this, but
-- doesn't have a convenient way to parse Maybe-ness in depth.

parseOpt :: FromJSON a => Value -> Text -> Parser (Maybe a)
parseOpt (Object o) key = o .:? key >>= maybe (return Nothing) (.: "value")
--}

-- now we need an intermediary for numeric encoding

data LangString = L { l', r', n' :: String, p1,g1,g2,g3 :: Maybe String }

instance FromJSON LangString where
   parseJSON o = L <$> parseVal o "languageLabel" <*> parseVal o "rootLabel"
                   <*> parseVal o "nspeakers" <*> parseOpt o "parentLabel"
                   <*> parseOpt o "grandLabel" <*> parseOpt o "greatLabel"
                   <*> parseOpt o "great2Label"

-- with that, we simply convert from LangString to Language

toLang :: LangString -> Language
toLang (L a b c d e f g) = Lang a b (read c) d e f g

-- and we formulate our query thusly:

queryLangRoots :: IO [Language]
queryLangRoots = map toLang . reifyWikiResults <$> sparql languageRootsQuery

{--
>>> head <$> queryLangRoots 
Lang {lang = "Mattole language", root = "indigenous languages of the Americas",
      nspeakers = 0, parent = Just "human language", grand = Just "language",
      great = Just "languoid", great2 = Just "abstract object"}

>>> length <$> queryLangRoots 
1036
 
I love that Haskell just works the first run because the compiler catches your
errors before they get into the runtime.
--}

-- We'll move the rest of this fun-part here to tomorrow's problem, as
-- parseOpt was a moral victory for me today. Rolling parseOpt into
-- Wikidata.Query.Aeson

{-- Moving below to tomorrow's problem:

-- Now, here comes the fun part.

-- 0. Construct a graph of languages from the results returned.

type Root = String

data LanguageGraph = WhatDoesYourLanguageGraphLookLike

-- hint: maybe look at the Regressive Imagery Dictionary graph?

languageGraph :: [Language] -> LanguageGraph
languageGraph langs = undefined

-- 1. What is/are the root node(s)?

roots :: [Language] -> [Root]
roots langs = undefined

-- 2. Which languages are in the greatest depth?

type Depth = Int

deepest :: [Language] -> (Depth, [String])
deepest langs = undefined

-- 3. Enumerate the roots from start to that 'deepest' language

pathTo :: [Language] -> String -> [String]
pathTo langs lang = undefined

-- 4. which roots have the greatest breath?

type Breath = Int

bigBreath :: {- eheh -} [Language] -> (Breath, [Root])
bigBreath langs = undefined

-- 5. Which roots have the most descendents?

type Count = Int

bigFamily :: [Language] -> (Count, [Root])
bigFamily langs = undefined

-- 6. Which roots have the most speakers of the various languages?

mostSpeakers :: [Language] -> (Count, [Root])
mostSpeakers langs = undefined

-- Tomorrow we'll look at a language-tree visualization.
--}

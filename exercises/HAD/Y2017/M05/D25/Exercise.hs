module Y2017.M05.D25.Exercise where

-- below imports available via 1HaskellADay git repository

import Data.Relation
import Graph.Query
import Graph.JSON.Cypher

import Y2017.M05.D24.Solution

{--
So, how far in depth do you go before you're 'done.'

There is a scientific approach to this if one has a tree or DAG with only
one root. Do we have this here?

Languages have roots, but language families have roots, too, and the families
of languages are in no way flat nor well-balanced.
--}

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

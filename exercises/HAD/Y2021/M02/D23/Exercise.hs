{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D23.Exercise where

{--
Name matching is the name of the game for today's Haskell problem.

You have a set of named wikidata wineries in "column A" and a set of named
graph wineries in "column B." You've already done the exact name-matches, now
how do we know which wineries in column A are the wineries in column B.

One way to do that is to by-hand pair the candidates. I had a CEO once that 
wanted to do name matches by-hand over thousands of articles, to base-line
our name-matching algorithm.

The operative phrase there is 'I HAD a CEO ONCE.'

The problem with by-hand matching is that it entails writing no software, so
you have (no joke) three secretaries working over 72 hours to come up with a
schedule for 100 people.

Or, you could do like me, and write the software that does the matching for
them in 1 second and prints out the schedule in PDF.

Then, like me, sell them that software.

The other, insidious, problem with by-hand matching is that is error-prone
because human beings are fatigue-prone, so, not only do you have to by-hand
name-match, but then you need to by-hand verify the by-hand name-matching.

Convert those hours into a bill, and you begin to see the immediate benefits
of a software solution.

Okay, so, today, let's use the double-metaphone name-matching algorithm. The one
I'm selecting to use here is written in Python.

For example: The Al Este winery in Argentina encodes as:

$ python metaphone.py Al Este
('ALST', '')

The repository is here:

https://github.com/oubiwann/metaphone

So, first we need to send a winery to metaphone and get back the encoding:
--}

import Data.Aeson hiding (KeyValue)

import Data.Set (Set)
import Data.Text (Text)

import System.Process

-- for the bonus problem:

import Y2021.M01.D29.Solution hiding (toPair)   -- Namei
import Y2021.M02.D22.Solution (wineriesWIP, NeoWinery(NeoWinery))
import Y2021.M01.D21.Solution (Idx, IxWineries)
import Y2021.M01.D22.Solution                   -- for wineries

import Data.Aeson.WikiDatum (Name)

doubleMetaphone :: Text -> IO (String, String)
doubleMetaphone winery = undefined

{--
Now, 16K+ wineries called one-by-one ... 'may' be a pain? But let's use
today's Haskell exercise to interoperate with other systems. Call the 
doubleMetaphone with at least one winery and show the result.

(That, of course, means you have the double-metaphone installed, so do that.
You'll also have to automate the double-metaphone application so you can call
it and get a result (writing a very simple __main__ function worked for me.)

That's it! That's today's Haskell exercise. DOIT! TOIT!
--}

{-- BONUS -------------------------------------------------------

All right, all right! A little bonus.

Output one of the winery-sets as names to a file, one winery per line.

I'll write a python script to scan that file and return the double-metaphone
encodings for each line.
--}

todaysDir :: FilePath
todaysDir = "Y2021/M02/D23/"

{-- ... but wait (see bonus-bonus-bonus below)
wineriesFile :: Namei a => FilePath -> Set a -> IO ()
wineriesFile outputFile wineries = undefined

>>> graphEndpoint >>= wineriesWIP (wineriesDir ++ wineriesJSON)
fromList [...]
>>> let (wikiws, graphws) = it
>>> (Set.size wikiws, Set.size graphws)
(481,16836)

>>> wineriesFile (todaysDir ++ "wiki-wineries.txt") wikiws
>>> wineriesFile (todaysDir ++ "graph-wineries.txt") graphws

-- BONUS-BONUS --------------------------------------------------

OR! You could just save out resulting file as el JSONerific.
--}

data KeyValue a b = KV a b
   deriving (Eq, Ord, Show)

instance (ToJSON a, ToJSON b) => ToJSON (KeyValue a b) where
   toJSON (KV a b) = undefined

data Metaphone = Meta (String, String)
   deriving (Eq, Ord, Show)

instance ToJSON Metaphone where
   toJSON (Meta (a,b)) = undefined

data IxKeyValue a b = IxKV Idx (KeyValue a b)
   deriving (Eq, Ord, Show)

instance (ToJSON a, ToJSON b) => ToJSON (IxKeyValue a b) where
   toJSON (IxKV ix kv) = undefined

toKV :: (Name, Idx) -> IO (IxKeyValue Name Metaphone)
toKV (n, ix) = undefined

{-- BONUS-BONUS-BONUS!! ----------------------------------------

I noticed a lot of QNames for Names in the wiki-winery data-set and a lot
of duplicates (triplicates, ... megalons (???)) in the graph-winery data-set
for their names. We don't need to process QNames nor (re)process multiples
for winery names, so filter all those out. ... but we also need to preserve
the index of these wineries, as well.
--}

instance Indexed Winery where
   ix _ = 0

instance Indexed NeoWinery where
   ix neo = undefined

removeQNames :: Indexed a => Namei a => Set a -> IxWineries
removeQNames = undefined

-- show that removeQNames 'automagically' removes duplicate names.

-- So, the updated wineriesFile is now:

wineriesFile :: Indexed a => Namei a => FilePath -> Set a -> IO ()
wineriesFile = undefined

-- show that removeQNames 'automagically' removes duplicate names.

{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D24.Solution where

{--
So, yesterday, we loaded in a slice of the NYT archive then saved it off as
JSON (the articles, proper) and haskell lists (the subject-linking information).

So, what are we going to do today?

Load back in that slice, obviously.
--}

import qualified Codec.Compression.GZip as GZ
import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)

-- below import available via 1HaskellADay git repository

import Y2017.M10.D23.Solution

-- first up: load in the subject linking information:

artIdsBySubj :: FilePath -> IO [Integer]
artIdsBySubj file = read <$> readFile file

{--
>>> floodsArtIds <- artIdsBySubj "Y2017/M10/D24/floods.lst" 
>>> length floodsArtIds 
156

>>> hurrArtIds <- artIdsBySubj "Y2017/M10/D24/hurricanes.lst" 
>>> length hurrArtIds 
173

>>> rainArtIds <- artIdsBySubj "Y2017/M10/D24/rains.lst"
>>> length rainArtIds 
84

--}

-- Now load in the articles, themselves, indexing by artId

instance FromJSON Article where
   parseJSON (Object o) = 
      Art <$> o .: "artIdx" <*> o .: "srcIdx"    <*> o .: "title"
          <*> o .: "author" <*> o .: "published" <*> o .: "abstract"
          <*> o .: "url"    <*> o .: "section"   <*> o .: "fullText"
          <*> o .: "people" <*> o .: "locations"

articlesFromFile :: FilePath -> IO [Article]
articlesFromFile file =
   fromMaybe [] . decode . GZ.decompress <$> BL.readFile file

{--
>>> artFloods <- articlesFromFile "Y2017/M10/D24/floods.json.gz" 
>>> length artFloods 
156
>>> (artIdx &&& title) $ head artFloods
(1650,"New Orleans Scrambles to Repair Drainage System After Severe Flooding")

>>> artHurricanes <- articlesFromFile "Y2017/M10/D24/hurricanes.json.gz" 
>>> length artHurricanes 
173
>>> (artIdx &&& title) $ head artHurricanes 
(321,"In Sweltering South, Climate Change Is Now a Workplace Hazard")

>>> artRains <- articlesFromFile "Y2017/M10/D24/rains.json.gz" 
>>> length artRains 
84
>>> (artIdx &&& title) $ head artRains 
(155,"How to Be Mindful Walking in the Rain")

Now that you have the articles by the topic you chose, partition the article
by subtopic, ... a bit of triage. For me, I choose "Hurricanes" so I'm 
partitioning articles by "Maria" "Harvey" "Irma" and none of those. For the
topic you choose you 'may' (and for this exercise 'may' means 'shall') choose
to partition your topic into subtopics (one of which being a catch-all.

How do we do this?

I'm thinking applicative functors... or something like them ...
--}

type Subcategory a = [a]

seed :: Subcategory Int
seed = [0,0,0,0] -- Maria, Harvey, Irma, none

-- For each article that mentions Maria, increment the Maria-count.
-- For each article that mentions Harvey, ...

-- etc, etc, and you get me

hasSubcategory :: String -> Article -> (Int -> Int)
hasSubcategory hurr art = if isInfixOf hurr (fullText art) then succ else id

-- from that, we get an applicative functor of hurricanes that we can <*>

-- How many articles do you have? How many of each subtopic do you have?
-- How many catchalls do you have (that is, no categorization?)

categorizor :: [Article] -> Subcategory Int
categorizor = foldr (\art -> 
   let maria  = hasSubcategory "Maria" art
       harvey = hasSubcategory "Harvey" art
       irma   = hasSubcategory "Irma" art
       none   = if maria (harvey (irma 0)) > 0 then id else succ
   in  zipWith ($) [maria, harvey, irma, none]) seed

    -- zipWith ($) via @SCourtenage and @clementd

{--
>>> cats = categorizor artHurricanes 
>>> cats
[19,117,86,18]
--}

-- And, finally, write out a report in human readable form of your subcategories

reportSubcategories :: Show a => [Article] -> Subcategory a -> IO ()
reportSubcategories arts [m,h,i,n] =
   putStrLn ("Out of " ++ show (length arts) ++ " articles:") *>
   putStrLn ('\t':show m ++ " articles are on Maria.") *>
   putStrLn ('\t':show h ++ " articles are on Harvey.") *>
   putStrLn ('\t':show i ++ " articles are on Irma.") *>
   putStrLn ('\t':show n ++ " articles are on none of the above.")

{--
>>> reportSubcategories artHurricanes cats
Out of 173 articles:
	19 articles are on Maria.
	117 articles are on Harvey.
	86 articles are on Irma.
	18 articles are on none of the above.
--}

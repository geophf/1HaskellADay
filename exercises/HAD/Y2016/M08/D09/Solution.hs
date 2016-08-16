{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2016.M08.D09.Solution where

import Y2016.M08.D08.Solution

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toUpper)
import qualified Data.Set as Set

import Graph.JSON.Cypher.Read.Rows

{--
Object (fromList [("favorites",Number 0.0),                    -- 3
                  ("id_str",String "727179491756396546"),
                  ("text",String "April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX"),
                  ("created_at",String "Mon May 02 16:54:35 +0000 2016"),
                  ("id",Number 7.271794917563965e17)])

data TweetGraph = TG [Value]
   deriving Show

instance FromJSON TweetGraph where
   parseJSON (Object obj) = TG <$> parseJSONList obj

--}

data Tweet = Tweet { idx, txt, created :: String, favs :: Integer }
   deriving (Eq, Ord, Show)

instance FromJSON Tweet where
   parseJSON (Object obj) =
      Tweet <$> obj .: "id_str"     <*> obj .: "text"
            <*> obj .: "created_at" <*> obj .: "favorites"

tweet :: TableRow -> Tweet
tweet (rowElements -> vals) = let (Success a) = fromJSON (vals !! 2) in a

{--
*Y2016.M08.D09.Solution> readJSONRows url ~> json
*Y2016.M08.D09.Solution> let tweets = map tweet json
*Y2016.M08.D09.Solution> head tweets ~>
Tweet {idx = "727179491756396546", txt = "April 2016 @1HaskellADay #haskell..."
--}

containsHashHaskell :: String -> Bool
containsHashHaskell = Set.member "#HASKELL" . Set.fromList . words . map toUpper

tweetsContainingHaskellTag :: [Tweet] -> Int
tweetsContainingHaskellTag = length . filter (containsHashHaskell . txt)

-- *Y2016.M08.D09.Solution> tweetsContainingHaskellTag tweets ~> 51

-- What are the tweets that do NOT have the #haskell/#Haskell hashtag?

noHaskellNahCry :: [Tweet] -> [Tweet]
noHaskellNahCry = filter (not . containsHashHaskell . txt)

{--
*Y2016.M08.D09.Solution> noHaskellNahCry ~> nothask
*Y2016.M08.D09.Solution> length nothask ~> 49
*Y2016.M08.D09.Solution> head nothask 
Tweet {idx = "753792336900067328",
 txt = "Published June 2016 @1HaskellADay #1Liner at https://t.co/aSbCufXm7G",
 created = "Fri Jul 15 03:24:32 +0000 2016", favs = 0}
--}

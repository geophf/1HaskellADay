{-# LANGUAGE OverloadedStrings #-}

module Y2016.M11.D11.Solution where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

-- below module available from cabal (aeson)

import Data.Aeson

{--
Okay, yesterday, we saw a structure. That structure being the network of 
followers shared by 1HaskellADay and this geophf twitter account, whoever
that is.

BUT WHO IS THIS geophf-PERSON! DISCERNING HASKELLERS MUST KNOW!

Well, we'll look at that today.

Today's Haskell exercise. I did an info-pull on this geophf-person by doing
the twitter API REST-call:

https://api.twitter.com/1.1/users/lookup.json?screen_name=geophf

with appropriate OAUTH via my twitter app (thanks to @abraham for the PHP
twitter query client).

So, with this file: geophfinfo.json on this directory or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D11/geophfinfo.json

parse out the following information:

screen_name, id_str, followers_count, friends_count (the 'following' count),
and user_mentions (which structure you must also declare).

Question: the users who mention geophf, are they friends with him? We may
look at this question tomorrow. Not today.
--}

type IdStr = String
class Ident a where
   id_str :: a -> IdStr

type Name = String
class Named a where
   name :: a -> Name

data Mention = Ment { screen_name, id_ment :: String } deriving Show

instance Ident Mention where id_str = id_ment
instance Named Mention where name = screen_name

data Entities = Ents { mentions :: [Mention] } deriving Show
data Status = LatestTweet { text, id_tweet :: String, ents :: Entities }
   deriving Show
data TweepInfo =
   Tweep { scr_nm, id_tweep :: String, followers, friends :: Int,
           stat :: Status }
      deriving Show 

instance Ident TweepInfo where id_str = id_tweep
instance Named TweepInfo where name = scr_nm

instance FromJSON TweepInfo where
   parseJSON (Object o) =
      Tweep <$> o .: "screen_name" <*> o .: "id_str" <*> o .: "followers_count"
            <*> o .: "friends_count" <*> o .: "status"

instance FromJSON Status where
   parseJSON (Object o) =
      LatestTweet <$> o .: "text" <*> o .: "id_str" <*> o .: "entities"

instance FromJSON Entities where
   parseJSON (Object o) = Ents <$> o .: "user_mentions"

instance FromJSON Mention where
   parseJSON (Object o) = Ment <$> o .: "screen_name" <*> o .: "id_str"

{--
Hint: be sparse in what information you pull from the JSON. You don't need to
parse everything, just the relevant bits.

'Relevant' is a technical term, it's precise definition is: 'whatevs.'

With the above declarations, read in the JSON parsing a set of TweepInfo.
--}

readTweepInfo :: FilePath -> IO [TweepInfo]
readTweepInfo = fmap (fromJust . decode) . BL.readFile

-- How many user mentions are in geophf's status?

{--
*Y2016.M11.D11.Solution> readTweepInfo "Y2016/M11/D11/geophfinfo.json" ~> geophf
[Tweep {scr_nm = "geophf", id_tweep = "81457442", followers = 2129, friends = 1675,
        stat = LatestTweet {text = "@ACatholicPrayer @KChasie I love Ps 51. I recite it every day of Lent.\nhttps://t.co/8RPXexSknv\nMiserere, Arvo P\228rt,\8230 https://t.co/yE9Au3PG8O", 
                            id_tweet = "796744304932876288",
                            ents = Ents [Ment {screen_name = "ACatholicPrayer",
                                               id_ment = "377775232"},
                                         Ment {screen_name = "KChasie", 
                                               id_ment = "4495736686"}]}}]

*Y2016.M11.D11.Solution> length . mentions . ents  . stat $ head geophf ~> 2
--}

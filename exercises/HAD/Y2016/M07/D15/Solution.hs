{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2016.M07.D15.Solution where

import Control.Monad ((>=>))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time

import Network.HTTP

import Data.Time.Calendar.Month (readTweetDate)

type ID = String

data Tweet = Tweet { tweetId :: ID, created :: String } deriving Show

-- so we simply create a FromJSON instance here

instance FromJSON Tweet where
   parseJSON = withObject "tweet" $ \t ->
      Tweet <$> t .: "id_str" <*> t .: "created_at"

dateFromTweet :: FilePath -> IO Day
dateFromTweet = readFile >=> \json -> let (Just tweet) = decode (BL.pack json)
   in pure (readTweetDate (created tweet))

{--
*Y2016.M07.D15.Solution> dateFromTweet "Y2016/M07/D15/tweet.json" ~>
2016-04-26
--}

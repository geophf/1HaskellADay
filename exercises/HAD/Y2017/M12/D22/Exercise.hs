{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D22.Exercise where

{--
Yesterday we read in JSON then we wrote out a subset of that JSON, today, and
the days following, we're going to parse that JSON, bit by bit.
--}

import Data.Aeson
import Text.HTML.TagSoup -- which you can get from cabal

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise (dir)

{--
Today we look at the 'content'-portion of the articles in the article JSON.

The content is a funny-lookin' guy, to quote Fargo, for it an HTML document
but broken up into a list of lines, offset by paragraph-tags (<p>).

Both? Why ont one or the other, but: 'the customer is always wrong.'

Did I mean to say 'the customer is always right'? No. No, I didn't.

So, we're going to parse in the subset from dir ++ "subset.json", and,
given that, extract each article's content.
--}

-- so, I'm rewriting the Article-type to get the content value. So let's
-- redeclare the Packet-type and generalize its rows-type

data Packet a =
   Pack { view :: String, count, total, next :: Int, prev :: Maybe Int, rows :: [a] }
      deriving (Eq, Show)

instance ToJSON a => ToJSON (Packet a) where
   toJSON packy = undefined

instance FromJSON a => FromJSON (Packet a) where
   parseJSON (Object o) = undefined

data Article = Art { uuid, title :: String, content :: [String] }
   deriving (Eq, Show)

instance FromJSON Article where
   parseJSON (Object o) = undefined

instance ToJSON Article where
   toJSON art = undefined

-- Now we can redeclare and redefine readSample on the new Article declaration

readSample :: FilePath -> IO (Packet Article)
readSample file = undefined

-- Now that you have the content as a list of strings we want two things

-- 1. the unparsed string as a block of HTML

htmlBlock :: Article -> String
htmlBlock art = undefined

-- 2. the parsed text as a set of lines of tag-free text

plainText :: Article -> [String]
plainText art = undefined

-- hint: use tagsoup to help here

-- What are the htmlBlock and plainText for each of the articles of subset.json?

{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D22.Solution where

{--
Yesterday we read in JSON then we wrote out a subset of that JSON, today, and
the days following, we're going to parse that JSON, bit by bit.
--}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import Text.HTML.TagSoup -- which you can get from cabal

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Solution (dir)

{--
Today we look at the 'content'-portion of the articles in the article JSON.

The content is a funny-lookin' guy, to quote Fargo, for it an HTML document
but broken up into a list of lines, offset by paragraph-tags (<p>).

Both? Why ont one or the other, but: 'the customer is always wrong.'

Did I mean to say 'the customer is always right'? No. No, I didn't.

So, we're going to parse in the subset from dir ++ "subset.json", and,
given that, extract each article's content.
--}

-- so, I'm rewriting the Article-type to get the content value

data Packet a =
   Pack { view :: String, count, total, next :: Int, prev :: Maybe Int, rows :: [a] }
      deriving (Eq, Show)

instance ToJSON a => ToJSON (Packet a) where
   toJSON packy = object (["view" .= view packy, "prev" .= prev packy,
                           "rows" .= rows packy]
         ++ zipWith (.=) (T.words "count total next")
                         ([count, total, next] <*> [packy]))

instance FromJSON a => FromJSON (Packet a) where
   parseJSON (Object o) =
      Pack <$> o .: "view" <*> o .: "count" <*> o .: "total"
           <*> o .: "next" <*> o .: "prev"  <*> o .: "rows"

readSample :: FilePath -> IO (Packet Article)
readSample file = fromJust . decode <$> BL.readFile file

data Article = Art { uuid, title :: String, content :: [String] }
   deriving (Eq, Show)

instance FromJSON Article where
   parseJSON (Object o) =
      Art <$> o .: "uuid" <*> o .: "title" <*> o .: "content"

instance ToJSON Article where
   toJSON art =
      object ["title" .= title art, "content" .= content art, "uuid" .= uuid art]

{-- and thus we have our content function making the below O.B.E
content :: Article -> [String]
content art = undefined
--}

{--
>>> pac <- readSample (dir ++ "subset.json")
>>> take 2 . content . head $ rows pac
["<h5>CHESAPEAKE</h5>",
 "<p>The City Council voted unanimously to approve changes that could reduce panhandling in the city.</p>"]
--}

-- content ... 'may' be facilitated by developing the Article-type

-- Now that you have the content as a list of strings we want two things

-- 1. the unparsed string as a block of HTML

htmlBlock :: Article -> String
htmlBlock = unlines . content

-- 2. the parsed text as a set of lines of tag-free text

plainText :: Article -> [String]
plainText = map (unwords . mapMaybe text . parseTags) . content

text :: Tag String -> Maybe String
text (TagText s) = Just s
text _           = Nothing

-- hint: use tagsoup to help here

-- What are the htmlBlock and plainText for each of the articles of subset.json?

art2art :: Article -> [String]
art2art art = "":([uuid, nl, title, nl] <*> [art]) ++ plainText art

nl :: Article -> String
nl = const ""

{--
>>> take 40 . htmlBlock . head $ rows pac
"<h5>CHESAPEAKE</h5>\n<p>The City Council "
>>> take 2 . plainText . head $ rows pac
["CHESAPEAKE",
 "The City Council voted unanimously to approve changes that could reduce panhandling in the city."]

And then, the piece d'resistance:

>>> writeFile ("Y2017/M12/D22/plainArts.txt") (unlines (concatMap art2art (rows pac)))
THANK you, tagsoup, for making HTML parsing out of the html content a breeze!
--}

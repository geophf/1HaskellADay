{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2018.M04.D24.Solution where

{--
Yesterday we saw that reading in the JSON and parsing it as Article' (and, I
saw, and Article) did not cause errors. But we also saw another interesting
result, and that is the text of the HTML (sometimes) contains HTML once the
HTML entities are decoded.

How do we know when we're done? How do we know how far down the rabbit hole we
have to go?

In other words, what's the fix-point of parsing HTML that may contain HTML
(which may contain HTML ...)?

Extracting the Article' (or Article) value from file as per Y2018.M04.D23,
deterime the fix-point of the de-HTML-ized values of the Article fields.
--}

import Control.Monad.Fix

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

-- below modules available via 1HaskellADay git repository

import Data.HTML (demark)

import Store.SQL.Connection

import Y2018.M04.D02.Solution   -- for Article' and Article
import Y2018.M04.D19.Solution   -- for ELT/Article insert
import Y2018.M04.D20.Solution (deHTMLize)
import Y2018.M04.D23.Solution   -- for json/article load

fixHTML2Text :: String -> String
fixHTML2Text html = fh2t' html html

-- BROKEN!!! --v

fh2t :: String -> String -> String
fh2t src (demark . deHTMLize -> dest) =
   if src == dest then src else fh2t dest dest

{--
The function fixHTML2Text extracts the text from HTML, even character encoded
text so that there's no HTML tags, encoded or otherwise in the resulting text.

Why is it a fix-point?

Because eventually it settles to a point where there's no more conversion to do.

Find that fixpoint.

Use either the file at Y2018/M04/D23 or download the JSON from the database
the fix-point your way to clean text of the Article fields.
--}

render :: (Article' -> Value) -> Article' -> String
render f art =
   let (Success m) = (fromJSON (f art)) :: Result (Map String Value) in
   let (String v) = m Map.! "rendered" in T.unpack v

{--
>>> (Just art) <- readSemiParsed trouble 
>>> fixHTML2Text (render excerpt art)
"*** Exception: (TagOpen "p" []) is not a TagText
CallStack (from HasCallStack):
  error, called at src/Text/HTML/TagSoup/Type.hs:97:17 
    in tagsoup-0.14.2-7gdPxgob4rD6glk9oZ3DIj:Text.HTML.TagSoup.Type

BOOM!

Because:

>>> take 70 $ render excerpt art
"<p><iframe frameborder=\"0\" height=\"135\" src=\"https://embed.iono.fm/epi"

works and:

>>> take 70 . demark $ render excerpt art
"<a data-cke-saved-href=\"http://iono.fm/e/360859\" href=\"http://iono.fm/"

so the fix-point is actually:

>>> demark . demark $ render excerpt art
"Content hosted by iono.fm  \n"
--}

fh2t' :: String -> String -> String
fh2t' src (demark -> dest) =
   if src == dest then src else fh2t' dest dest

-- the fixHTML2Text now works on excerpt and content with the new fh2t'

{--
>>> withConnection WPJ (\c -> loadSemiParsed c [Idx 461] >>=
                              insertArticles c . map (IxV 461) >>= print)
[Idx 462]

And the proof is in the pudding: the insert is working for the previous broken
insert at the article ID.
--}

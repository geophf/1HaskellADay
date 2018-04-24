{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D24.Exercise where

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

-- below modules available via 1HaskellADay git repository

import Y2018.M04.D02.Exercise   -- for Article' and Article
import Y2018.M04.D20.Exercise (deHTMLize)
import Y2018.M04.D23.Exercise   -- for json/article load

fixHTML2Text :: String -> String
fixHTML2Text html = undefined

{--
The function fixHTML2Text extracts the text from HTML, even character encoded
text so that there's no HTML tags, encoded or otherwise in the resulting text.

Why is it a fix-point?

Because eventually it settles to a point where there's no more conversion to do.

Find that fixpoint.

Use either the file at Y2018/M04/D23 or download the JSON from the database
the fix-point your way to clean text of the Article fields.
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D29.Exercise where

{--
So, the wikipedia entry for all alliances of the world:

https://en.wikipedia.org/wiki/List_of_military_alliances

has a section on modern, or current, military alliances. These entries
are saved in the wikitext-format. The wikitext format is described here:

https://meta.wikimedia.org/wiki/Help:Wikitext_examples

... but is self-evident from the text, itself.

The listing of modern military alliances is archived here:
--}

dear :: FilePath
dear = "Y2020/M10/D29/"    -- ... geddit?

moderns :: FilePath
moderns = "modern-alliances.wtxt"

-- figure out how to scan and parse the above document into the below
-- data structure



module Data.HTML where

-- Describes a thing that has a body of content.  No. Really.

import Data.Maybe (mapMaybe)
import Text.HTML.TagSoup -- which you can get from cabal

class HTML a where
   body :: a -> [String]

-- 1. the unparsed string as a block of HTML

htmlBlock :: HTML a => a -> String
htmlBlock = unlines . body

-- 2. the parsed text as a set of lines of tag-free text

plainText :: HTML a => a -> [String]
plainText = map demark . body

-- 3. gets the content (text) for an element, removing the element tags

demark :: String -> String
demark = unwords . mapMaybe text . parseTags

text :: Tag String -> Maybe String
text (TagText s) = Just s
text _           = Nothing

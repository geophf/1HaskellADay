module Y2017.M01.D10.Exercise where

import Data.Binary
import Data.ByteString.Lazy.Char8
import Data.Set (Set)
import Data.Text
import Network.HTTP.Conduit

{--
Today, we'll look at another Haskell pitfall from @bitemyapp, the String.

Strings are natural
String are fun
Strings are best when they're
One. on. One.

But some people don't like that String's underlying representation is a linked
list as opposed to an array. This is bad space-utilization and makes String
indexing O(n) instead, if it were array-based, O(1).

Okay, so let's look at an alternative to String, which is Data.Text.

Read in the most popular book on gutenberg.org, which is Christmas Carol:

http://www.gutenberg.org/cache/epub/46/pg46.txt

And measure the size of the value downloaded as a String and then downloaded
as Text. What is the space-savings?
--}

type URL = FilePath

ebook2String :: URL -> IO String
ebook2String url = undefined

ebook2Text :: URL -> IO Text
ebook2Text url = undefined

-- Of course, you'll have to convert the downloaded book from ByteString to
-- firstly, String, then secondly, Text to do this. Converting ByteString to
-- String is trivial. How does one convert a ByteString to Text?

byteString2Text :: ByteString -> Text
byteString2Text = undefined

-- Which is larger? Christmas Carol as a String or as Text?

lengthOf :: Binary a => a -> Int
lengthOf book = undefined

-- hint: Thread on stack overflow on how to measure lengths of Haskell values

-- Sort the words in Christmas Carol, group them, answer the below:

-- 1. what is the most-used word(s) in Christmas Carol?

type Count = Int

wordFrequency :: Text -> [(Text, Count)]
wordFrequency book = undefined

-- 2. what is the longest word(s) in Christmas Carol

type Length = Int

wordLengths :: Text -> Set (Length, Text)
wordLengths = undefined

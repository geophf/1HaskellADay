module Y2017.M01.D10.Solution where

import Control.Arrow ((&&&))
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sort, group, sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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
--}

type URL = FilePath

bookURL :: URL
bookURL = "http://www.gutenberg.org/cache/epub/46/pg46.txt"

{--
And measure the size of the value downloaded as a String and then downloaded
as Text. What is the space-savings?
--}

ebook :: URL -> IO ByteString
ebook = simpleHttp

-- *Y2017.M01.D10.Solution> ebook bookURL ~> book
-- *Y2017.M01.D10.Solution> BL.length book ~> 181997

ebook2String :: URL -> IO String
ebook2String = fmap BL.unpack . ebook

ebook2Text :: URL -> IO Text
ebook2Text = fmap byteString2Text . ebook

-- Of course, you'll have to convert the downloaded book from ByteString to
-- firstly, String, then secondly, Text to do this. Converting ByteString to
-- String is trivial. How does one convert a ByteString to Text?

byteString2Text :: ByteString -> Text
byteString2Text = T.pack . BL.unpack

-- *Y2017.M01.D10.Solution> let tbook = byteString2Text book

-- but I mean, you show me a better way

-- Which is larger? Christmas Carol as a String or as Text?

lengthOf :: Binary a => a -> Int
lengthOf = fromIntegral . BL.length . encode

-- length as String
-- *Y2017.M01.D10.Solution> lengthOf (BL.unpack book) ~> 182010

-- length as Text
-- *Y2017.M01.D10.Solution> lengthOf (byteString2Text book) ~> 182010

-- huh. Huge difference. Huge.

-- Sort the words in Christmas Carol, group them, answer the below:

-- 1. what is the most-used word(s) in Christmas Carol?

type Count = Int

wordFrequency :: Text -> [(Text, Count)]
wordFrequency = sortBy (compare `on` snd)
              . map (head &&& length) . group . sort . T.words

{--
*Y2017.M01.D10.Solution> take 5 (reverse $ wordFrequency tbook)
[("the",1568),("and",1042),("of",756),("to",709),("a",709)]

Gripping Dickensian prose!
--}

-- 2. what is the longest word(s) in Christmas Carol

type Length = Int

wordLengths :: Text -> Set (Length, Text)
wordLengths = foldr (Set.insert . (T.length &&& id)) Set.empty . T.words

{--
*Y2017.M01.D10.Solution> let setwords = wordLengths tbook
*Y2017.M01.D10.Solution> take 5 (reverse $ Set.toList setwords)
[(30,"http://www.gutenberg.net/4/46/"),(30,"http://gutenberg.net/license)."),
 (29,"http://pglaf.org/fundraising."), (24,"snowball--better-natured"),
 (24,"http://www.gutenberg.net")]

So, el-Charlie-o likes using URLs in his manuscripts. Huh. Who'da thunk?

Charles Dickens, like Charles Darwin, both men Charles, both men C.D.,
both men men. Both men way before their time!
--}

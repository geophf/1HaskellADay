{-# LANGUAGE OverloadedStrings #-}

module Y2018.M07.D09.Solution where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Data.Maybe (fromJust)

-- Today we look at some JSON:

exDir, mondoJSON :: FilePath
exDir = "Y2018/M07/D09/"
mondoJSON = "vp_new_280k_july5.json"

{--
The structure of the JSON is ...

Actually, I don't know what the structure of this JSON is. It's big, it's all
in one line, and it's not what the structure is reported to be.

What it's reported to be is this:

* vp280kcomplete_ALL : {'Totals': blah_dict , 'Per_Article': glah_dict}
* vp280kcomplete_ALL['Totals']  --> {all extracted entities and the document 
  uid in which they are found along with the individual document frequencies}
* vp280kcomplete_ALL['Totals']['entity_blah']['Total'] --> inverse doc 
  frequency of entity = 'entity_blah'
* vp280kcomplete_ALL['Totals']['entity_blah']['Article_Freq'] --> articles the 
  entity occurs in and the frequency in given document
* vp280kcomplete_ALL['Per_Article'] --> key into particular uid and the value 
  is another dictionary of the entities in that article and the document 
  frequency

But you see from the first few characters:

{"Total": {"": {"Total": 15076828, "Articles_Freq": {"269066": 1150, ...

That the reported structure and the actual structure do not match.

Today's Haskell problem. Prettify the above JSON input so we can start to
see what the actual structure of the JSON is.
--}

toMappage :: ByteString -> Map String Value
toMappage = fromJust . decode

prettify :: FilePath -> IO ()
prettify file =
   BL.readFile file >>= putStrLn . take 300 . BL.unpack . encodePretty . toMappage

{--
>>> prettify (exDir ++ mondoJSON)
{
    "Per_Article": {
        "270382": {
            "": 1648
        },
        "275206": {
            "": 580
        },
        "263193": {
            "": 1140
        },
        "271392": {
            "": 1734
        },
        "268091": {
            "": 1752
        },
        "262673": 
--}

{-- BONUS -----------------------------------------------------------------

So, we know some of the actual structure from the first few characters:

"Total" -> "" -> { ("Total", int), ("Articles_Freq" -> {(String,int)}) }

Is there more structures in the JSON? What is it?
--}

structures :: FilePath -> IO ()
structures json = undefined

-- output your results as a hierarchy of structure

{--
So answering prettify answered structures, we have the "Total" structure
(by inspecting the raw JSON), and now we have one more key-value pairing:

"Per_Article" -> Map String (Map String Integer)

Verified with:

>>> json <- BL.readFile (exDir ++ mondoJSON)
>>> mapo = toMappage json
>>> length mapo
2
>>> Map.keys mapo
["Per_Article","Total"]

TA-DAH!
--}

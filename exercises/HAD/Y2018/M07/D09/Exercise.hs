{-# LANGUAGE OverloadedStrings #-}

module Y2018.M07.D09.Exercise where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)

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

prettify :: FilePath -> IO ()
prettify json = undefined

{-- BONUS -----------------------------------------------------------------

So, we know some of the actual structure from the first few characters:

"Total" -> "" -> { ("Total", int), ("Articles_Freq" -> {(String,int)}) }

Is there more structures in the JSON? What is it?
--}

structures :: FilePath -> IO ()
structures json = undefined

-- output your results as a hierarchy of structure

{-# LANGUAGE OverloadedStrings #-}

module Y2021.M04.D07.Solution where

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)

-- Okay, I ran across a problem with decode. Try it on this file:

exDir, jsonFile :: FilePath
exDir = "Y2021/M04/D07"
jsonFile = exDir ++ "/bigOle.json"

type DBTable = Map String Int

readDBTable :: FilePath -> IO (Maybe DBTable)
readDBTable file = decode <$> BL.readFile file

-- what result do you get?

{--
>>> readDBTable jsonFile 
Nothing

Now, image that there are 100s of thousands of entries, and the weirdness
happens in 1 row, buried somewhere, deeply, in the data-set.

This happened to me.

And this is when I discovered eitherDecode.

Today's Haskell exercise: use eitherDecode to give Either the error of where
the JSON failed to parse OR the DBTable:
--}

fetchDBTable :: FilePath -> IO (Either String DBTable)
fetchDBTable file = eitherDecode <$> BL.readFile file

-- What result do you get?

{--
>>> fetchDBTable jsonFile 
Left "Error in $.e: parsing Int failed, expected Number, but encountered String"
--}

{-- BONUS -------------------------------------------------------

Using the information from the error message above (SPOILER!), create a new,
corrected, JSON and refetch. What do you get?

>>> fetchDBTable (exDir ++ "/corrected-bigOle.json")
Right (fromList [("a",1),("b",2),("c",3),("d",4),("e",5),("f",6),("z",12)])
--}

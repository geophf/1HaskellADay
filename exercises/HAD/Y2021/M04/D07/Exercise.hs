{-# LANGUAGE OverloadedStrings #-}

module Y2021.M04.D07.Exercise where

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
Now, image that there are 100s of thousands of entries, and the weirdness
happens in 1 row, buried somewhere, deeply, in the data-set.

This happened to me.

And this is when I discovered eitherDecode.

Today's Haskell exercise: use eitherDecode to give Either the error of where
the JSON failed to parse OR the DBTable:
--}

fetchDBTable :: FilePath -> IO (Either String DBTable)
fetchDBTable = undefined

-- What result do you get?

{-- BONUS -------------------------------------------------------

Create a new, corrected, JSON and refetch. What do you get?
--}

{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D23.Exercise where

{--
Okay, we're doing a little hello-echo stuff today.

Create a program that accepts a string argument, and returns JSON of that
string
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString.Lazy.Char8 as BL

data Name = Im String
   deriving (Eq, Show)

instance ToJSON Name where
   toJSON namuchan = undefined

main' :: [String] -> IO ()
main' args = undefined

-- Why am I doing this? So I can prove I can talk to PHP and I can read
-- PHP's return in JSON with AJAX, but you don't have to do that (if you don't
-- want to), just return a JSON-y string, is all.

{-# LANGUAGE OverloadedStrings #-}

module Y2018.M07.D17.Exercise where

{--
Another day. Another JSON.

Remember, back in the day, when everything was all about XML? This was back in
1996 and there was an XML-craze of all data had to be in XML or it wasn't cool.

I get a similar vibe about [the current year] and JSON.

So, anyway. We have a JSON that has articles and data and metadata on articles.
From this JSON extract the Article ID (the UUID) and the preview image of the
article as an URL.
--}

import Data.Aeson

data Article = Art { uuid :: String, previewImg :: Maybe FilePath }
   deriving Show

instance FromJSON Article where
   parseJSON json = undefined

previews :: FilePath -> IO [Article]
previews file = undefined

-- How many articles are in the JSON?
-- How many articles have preview images?

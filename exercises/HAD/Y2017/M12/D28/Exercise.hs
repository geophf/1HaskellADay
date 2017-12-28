{-# LANGUAGE QuasiQuotes, TypeSynonymInstances #-}

module Y2017.M12.D28.Exercise where

{--
Just a quicky for today.

Yesterday's solution had something like this for a runoff of sample.json parsing

Parsed e9c76f0e-92d3-5a17-8450-ffa3516bfc83
Parsed 7740ba62-72a6-52c4-81ae-8140c9e93d70
Could not parse article 20, error: expected String, encountered Null
Parsed 81bddb98-fc1a-53fa-b6d6-19bc63a8986c
Could not parse article 22, error: expected String, encountered Null
Could not parse article 23, error: expected String, encountered Null
Parsed 62f6f2ec-2927-55a4-9009-4794c1d9bc17
Parsed a8c6649e-d519-5c92-9317-652a4ef63d11
Parsed 099bdf9b-1b1f-5bb2-9299-cef67e1d65c7
Could not parse article 27, error: expected String, encountered Null
Parsed 905a8c8c-cba9-5ec1-b16e-4d8f4b412070

So, what are these optional strings?

Well, we can find them programatically or we use our eyeballs to scan the JSON.

Let's go with the latter today.
--}

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise

fetchBlockStmt :: Query
fetchBlockStmt = [sql|SELECT block FROM article_stg WHERE id=?|]

fetchBlocks :: Connection -> [Integer] -> IO [Block]
fetchBlocks conn ixs = undefined

-- question: how do you convert the string returned into a Block value?

instance FromRow Block where
   fromRow = undefined

-- with fetch blocks, fetch the blocks and print them out

printBlocks :: [Block] -> IO ()
printBlocks blocks = undefined

-- examine the blocks that did not parse. What fields (that DatedArticle cares
-- about) are optional?

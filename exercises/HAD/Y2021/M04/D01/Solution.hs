{-# LANGUAGE OverloadedStrings #-}

module Y2021.M04.D01.Solution where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import qualified Data.Text as T

import Network.HTTP.Req

{--
Hi. Good morning, all.

Today's Haskell problem. We're going to get some data from a web endpoint.

The GET examples for Network.HTTP.Req have you got to httpbin.org and get
some random bytes.

Now, I'm not SAYING you have to use the Req library. In fact, what I'm SAYING
is this: use whatever library you like to do the task. That task?

Go to httpbin.org/bytes/5?seed=100 and get five random byte and display
their values here.
--}

fiveRandomBytes :: IO ()
fiveRandomBytes = main :: IO ()
main = runReq defaultHttpConfig $ do
  let n, seed :: Int
      n    = 5
      seed = 100
  bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse $
    "seed" =: seed
  liftIO $ print (map ord (B.unpack (responseBody bs)))

{--
>>> fiveRandomBytes 
[74,235,232,89,201]
--}

{--
NOW, if you DO want to use the Req example in the documentation to do this,
I'm not going to stop you (trans.: please do use the documentation).
--}

-- The 'fun' part -------------------------------------------------------

{--
Okay, here's where the fun begins.

If you go to a bad URL, the Req library defaults to throwing an error, but
today's Haskell exercise doesn't want you to do that. Instead, what I'd like
you to do is this: go to a wrong URL, and if that fails (or, if it doesn't
succeed, to put it another way), then, instead, display some helpful message
to your client, like: "Website not found. Would you like to play a game of
chess?"

For, we all know that the WHOPR beat the Big Mac.

And global thermonuclear war is less fun that chess.

*ahem* So, instead of going to the above URL, go, instead to

https://httpbin.org/sed/4?sedes=mustard

to get that big, ol' 404 both you and I are cravin'

... and NOT throw an exception, but instead prints out a User Friendly Message.
--}

whopr :: IO ()
whopr = 
  let noThrow = \_ _ _ -> Nothing in
  runReq (defaultHttpConfig { httpConfigCheckResponse = noThrow }) $ do
  let n :: Int
      n       = 4
      moutard = T.pack "mustard"
  bs <- req GET (https "httpbin.org" /: "sed" /~ n) NoReqBody bsResponse $
    "sedes" =: moutard
  liftIO $ if responseStatusCode bs == 200
           then print (map ord (B.unpack (responseBody bs)))
           else putStrLn "Website not found. Would you like to play a game of chess?"

{--
>>> whopr 
Website not found. Would you like to play a game of chess?
--}

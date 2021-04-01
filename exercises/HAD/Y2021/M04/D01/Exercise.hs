{-# LANGUAGE OverloadedStrings #-}

module Y2021.M04.D01.Exercise where

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
fiveRandomBytes = undefined

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
whopr = undefined -- "beat the big mac."

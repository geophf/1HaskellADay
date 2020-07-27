module Y2002.M07.D27.Exercise where

{--
Today's exercise is "Haskell, and ..."

"Haskell, and what?" you ask reasonable.

So.

Haskell can build complete systems and execute them independent of that great,
big world of ... well, whatever: one year it was C/C++, another it was Java,
maybe this year it's Python or Scala.

Or whatever.

Let's say you have another application, call it "the_wall" whose job is to
launch all nuclear missiles, or, more pragmatically, initiate a whole-world-
shutting-down virus (... too soon?), then, upon completion of this launch,
print out: "Goodbye, cruel world."

Because Cyberdyne, and stuff.

Or, we can skip the cataclyzmic world-ending events, and just print out the
message, and PRETEND we've dominated the world, for funzies, and for the
interest of brevity of this exercise.

And non-world-ending, too, because we're nice.

Anyway. Call the external-not-written-in-Haskell app, redirect the output
of that application and return that output as the result of that ...

... dare I call it a 'function call'?

... 'function call.'

Now, I didn't say: 'functional call'; I said: 'function call.'
--}

exerciseDir :: FilePath
exerciseDir = "Y2020/M07/D27/"

externalApp :: FilePath
externalApp = "the_wall.py"

callOut :: FilePath -> IO String
callOut app = undefined

{--
p.s.: You see the_wall.py is a python app, so you can install python app if
you don't have it, or you can replace my example external app with any kind
of app that works for you. Say: use FORTH, instead, or ... FORTRAN, or ... F#.
--}

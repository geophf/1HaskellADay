module Y2021.M03.D05.Exercise where

{--
'Shell scripting' Haskell is the name of the game for today.

Today we're going to build a Haskell application that interacts with the
shell, or 'command line,' if you prefer.

Specifically, we're creating the application 'im' that has this interaction:

$ im geophf
Hello, geophf! How are you?

$ im fine
That's good to hear. I hope you have a lovely day!

Apologies to all of you Haskellers who go by the name 'fine.'

You can even name this module 'Eliza,' if you're feeling nostalgic.
--}

greet :: String -> IO ()
greet name = undefined

-- greet, for 'fine' responds with the well-wishes for a good day, otherwise
-- asks 'name' how they are.

-- use the file im.hs in this directory to create your shell ... um: 'script.'

-- BONUS -------------------------------------------------------

-- Now ... you can build a database to see if they were first asked and
-- tailor (swift?) the response based upon that context, if you want to get
-- fancy.

-- BONUS-BONUS -------------------------------------------------

-- What if they don't enter anything? Add an errorHandler to guide them,
-- gently, into this dialogue.

{--
$ im
Hi. What's your name?
$ im geophf
Hello, geophf! How are you?
$  im fine
That's good to hear. I hope you have a lovely day!
--}

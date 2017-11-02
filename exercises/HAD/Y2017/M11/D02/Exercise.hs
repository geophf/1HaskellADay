module Y2017.M11.D02.Exercise where

{--
Here's a simple Haskell exercise for you on All Souls' Day.

Create an application, call it im, that takes as arguments your name and
replies with a friendly greeting.

For example:

$ im geophf
Hello, geophf, how are you?

$ im
What? You're what? Are you going to tell me your name or what?

... something like that.
--}

main' :: [String] -> IO ()
main' name@(_:_) = undefined  -- greet name
main' [] = undefined          -- prompt for name

-- Now, at the top level directory have a module, call it Main, that calls
-- this main' function with the command line arguments.

-- There you go. Bob's your uncle.

module Y2017.M10.D11.Exercise where

{--
Today we're going to isolate a little problem I'm having. You see, and you may
not know this, but not all data is pristine.

SHOCKER! I KNOW!

So, what happens when you run into dirty data? Well, the system crashes, because
you built it perfectly, or so you thought, but then the dirty data shows you 
that your guard clauses weren't guardy enough, nor clausy enough.

Well, I looked at the logs:

ETL process: start.
*** Exception: For article 221: Key should be "Abstract" but is actually "Publication title"

The problem is, in my code, I index articles 1, 2, ... because the index doesn't
matter...

Well, it turns out, in a week's archive of 1000 articles, knowing the index in
the text can be REALLY helpful in debugging the problem.

I mean, like: REALLY helpful.

So: today's Haskell problem is a "Document x of y"-parser.

You have this header for each article, a line of underscores (which we use
to separate articles in the archive, so that's good) then this line:
--}

header :: String
header = "Document 102 of 999"

-- parse that line and return x in "Document x of y"

parseHeader :: String -> Integer

{--
... but hold your horses there, pahdnah. What happens when everything goes
South, as it did for me on document 221?

Do we fail hard and cause the ETL to crash?

Well, yes. Yes, in fact, we do just that, looking at Y2017.M09.D25.Exercise.

Ah, good, then. smh.
--}

parseHeader header = undefined

{-- BONUS -----------------------------------------------------------------

Parsing.

They write books on this kind of stuff. Do you know I have an automata book
that cost somebody $125 to buy in college? I bet you there's not one partial
function in that entire book!

Yay.

Write a better parser for parseHeader, so that it returns either the
anticipated value or it returns NOT the anticipated value, but a reason
why parsing "aflkjalhkghaweoifw0" does not get you a document number.
--}

data Parser a = SomeKindOfParserOn a

parseHeaderM :: String -> Parser Integer
parseHeaderM header = undefined

leGarbage :: String
leGarbage = "afdkjkgkgkjlaglkjjsdfdlkjkfds"

-- Now parse header. What is your result?
-- Parse leGarbage. What is your result?

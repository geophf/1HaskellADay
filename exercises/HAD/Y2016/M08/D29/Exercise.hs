module Y2016.M08.D29.Exercise where

{--
Now for something completely different.

My daughter, EM, posed this #math problem to me. Can you solve it?

WHAT IS THE NEXT NUMBER?
3
13
1113
3113
132113
?
--}

series :: [Integer]
series = map read (words "3 13 1113 3113 132113")

nextInSeries :: [Integer] -> Integer
nextInSeries = undefined

{-- BONUS -----------------------------------------------------------------

Now that you solved the above ...

... you have solved the above ...

J. M. Varner @JMVarnerBooks laments: "Not a very efficient encoding scheme ;)"

And posits that 'DEFLATE' is an effient one... whatever 'DEFLATE' is.

So, devise an efficient encoding scheme for the above series. Share.
--}

data Encoding = EhIDunnoYouTellMe

encode :: [Integer] -> Encoding
encode = undefined

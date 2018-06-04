module Y2018.M06.D01.Exercise where

{--
Today, in Haskell, we're going to ... ADD TWO NUMBERS!

STOP THE PRESSES!

Okay, here's the thing.

"How do you put Haskell up on AWS Lambda, ooh! So hard! Such Wow!" and such.

Welp, if you have everything contained in Haskell, all you need to do is
call the Haskell application with a AWS Lambda-friendly language (there are
a few of them, including Javascript and Python), package all that together in
one bundle, and BOOM! you have a Haskell AWS Lambda function.

So, today, I provide the Python wrapper, you provide the Haskell application,
and, together we will RULE THE EMPIRE AS FATHER AND SON!

No, wait: wrong Galaxy. Sorry.

Write a main function that takes the two command line arguments and sums them,
printing the result on your standard output stream (we call it the console or
the screen ... or you could use an abacus, so long as it runs Haskell).
--}

import System.Environment (getArgs)

main :: IO ()
main = undefined

-- now, compile this to an application named adder, and run the python 
-- application toy.py

{--
$ ./adder 3 4
3 + 4 = 7

$ python toy.py
{'result': '3 + 4 = 7'}
--}

{-- BONUS -----------------------------------------------------------------

Zip the adder executable and toy.py. Using the AWS CLI, upload the bundle to
AWS Lambda, creating the Lambda function. Call it with whichever REST client
you like, then go around to all your friends, for you now have a Haskell AWS
Lambda function for great good.

The aws cli command will look something like this:

$ aws lambda create-function --region [your region] --function-name toy \
       --zip-file fileb:[path-to-bundle]/hask.zip --handler toy.handler \
       --runtime python3.6 --timeout 60 --memory-size 1024 --role [your ARN]

You MAY need to get haskell-platform installed on an AWS EC2 and create the
Haskell executable there. I had to do it that way, anyway.
--}

module Y2020.M07.D28.Solution where

{--
Father: Once, long ago, there was a development environment called 'netbeans'
where you didn't write any code at all, ever, you dragged and dropped components
and connected the-...
Mother: Hush, dear! You're scaring the children!

So, that has nothing to do with anything ...

... except, more than 10 years later, I had to debug a netbeans 'Enterprise'
app, and the only way anything would work was to download netbeans itself, and
view the system in the IDE.

Ugh.

Anyway: the 'code without coding!'-crowd? that show up every six years or so?

Yeah. Don't listen to them.

So, like yesterday, let's say you have an external app that you don't want to
rewrite, you just want to use, let's say it's even a netbeans app, but, unlike
yesterday, this app takes arguments, and we need to verify:

1. that it returned successfully; and,
2. that we get back expected results.

Your mission, should you decide to accept it, is to call this external 
application, and do the above verifications.
--}

import System.Process
import GHC.IO.Exception   -- .ExitCode

exerciseDir :: FilePath
exerciseDir = "Y2020/M07/D28/"

app :: FilePath
app = "summer.py"

callUndVerify :: FilePath -> String -> IO String
callUndVerify exe arg = 
   readProcessWithExitCode exe [arg] "" >>= \(code, ans, _) ->
   return (let errOut = "Whazzamattahfur you? I can't do no sum o' no " ++ arg
               answer = chomp ans
           in  if code == ExitSuccess && verify arg answer 
               then answer
               else errOut)

verify :: String -> String -> Bool
verify "10" "45" = True
verify _    _    = False

chomp :: String -> String
chomp str = let (h:t) = reverse str
            in  if h == '\n' then reverse t else str

{--
So: verify that calling the app with 10 returns 45, and calling the app with
123 returns an error from the external app.

p.s.: Also, since you know this is a python-app, you can either rely on the
script's designation of what python is, or you can specify python from your
execution of the app here. Your choice.

>>> callUndVerify (exerciseDir ++ app) "10"
"45"

>>> callUndVerify (exerciseDir ++ app) "101"
"Whazzamattahfur you? I can't do no sum o' no 101"

>>> callUndVerify (exerciseDir ++ app) "frankenfurters"
"Whazzamattahfur you? I can't do no sum o' no frankenfurters"
--}

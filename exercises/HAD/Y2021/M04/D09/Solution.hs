module Y2021.M04.D09.Solution where

import Data.Time

{--
Well, howdy!

Today's Haskell exercise is to write a daily report that goes to your CFO
for review and signature.  You don't have a CFO? You don't know what a CFO
even is?

All the better for you. Trust me on that one.

So, CFOs love getting ... oh, 243 daily reports, each report 200+ pages that
he, or she, has to review and sign personnally, because how else can they
justify their paycheck?

How do CFOs read 400,000 pages every single morning? Because all 243 reports
are deemed 'critical,' by the way?

I have no earthly idea. I think CFOs should learn to code, but that's just
me.

So, the report you're sending to the CFO is the TPS report, of course, and
you have to have the cover letter on the TSP report, because otherwise you
didn't get the memo on that, and I'll get you another copy of that memo.

The irony of the movie "Office Space" is that it 100% based on actual day-to-
day events that go on in a nearly uncountable number of offices in the
Washington, D.C. area alone.

So, your cover letter contains the text:
--}

coverLetterBody :: [String]
coverLetterBody = [
   "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ",
   "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim ",
   "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea ",
   "commodo consequat. Duis aute irure dolor in reprehenderit in voluptate ",
   "velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat ",
   "cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id ",
   "est laborum."]

-- you need to date this report (with today's date, obviously) and save it

generateReport :: FilePath -> Day -> [String] -> IO ()
generateReport toFile date body =
   writeFile toFile (unlines (officiousHeader date ++ coverLetterBody))

-- Also, add an officious ... sry: OFFICIAL! ... header to your document

officiousHeader :: Day -> [String]
officiousHeader date = ["From: el geophf, le munificent",
   "To: CFO Grande Poombah",
   "RE: Cover letter for TPS-reports, according to Memo 48372.c.387.3.d",
   "Date: " ++ show date, ""]

-- Make it look all Governmental and officio--.. sry: OFFICIAL!

-- sheesh

-- do it. to it.

{--
>>> getCurrentTime >>=
    flip (generateReport "Y2021/M04/D09/DailyReport.txt") coverLetterBody
    . utctDay

Generates the generated cover letter that covers the report ... that is 
generated. Daily. And has a cover letter. Did I mention that? Or did you not
get that memo? I'll make a copy of that memo for you.
--}

module Y2016.M06.D28.Exercise where

import System.Environment


{--
So, I was struggling both Sunday and Monday as to what would be this week's 
theme for the Haskell exercises, and there it was staring at me in the face
the whole time.

Work.

So you would not believe the 'not invented here'-mentality that pervades Big
Gov't. If 'they' didn't write the code, then it doesn't even exist in their
minds. Web Services? REST? They've heard of these things, but it might be
risky. Maybe. And 'cloud computing.'

Big Gov't has 'cloud computing.' Sure they do: it's a mainframe in their 
basement that is inaccessable outside their VPN.

Yes, they actually do have VPN. Shocker!

So, today's task.

Read in a file ... BUT WAIT! WHAT IF THE FILE IS NOT THERE? PANIC-TIME! WE NEED
SECURITY MEASURES IN PLACE.

So, don't even read in a file. Do THIS instead:

Write a Haskell program that checks to see if a file is 'there' (where it's
supposed to be). If it is, well, then do nothing and exit 0 (all is well).

If the file is NOT there, then return to the shell the 'FILE NOT FOUND' result
(which happens to be, for this scenario, 4).

(Yes, you read that correctly: return 4 to the shell if the requested file is
not found).

So, interaction with the (operating) system is on the plate for today.

How do you call the program? Your first argument will be the file that should
be at your directory, so reading from the (operating) system when your Haskell
program is invoked is another thing you have to do.

At this directory is a file named Schnitzengruben.txt. Look for it. Nothing
should happen (meaning 0 is returned to the operating system), now look for
a file names Schatzie.txt. Nothing should happen, again, other than the fact
that 4 is returned to the operating system.

Have at it!
--}

main = undefined

-- Schnitzengruben.txt is at this directory and at the URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D28/Schnitzengruben.txt

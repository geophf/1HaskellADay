module Y2018.M12.D19.Exercise where

{--
So yesterday, you were given a list of positive and negative numberd and told
to solve the sum-distance from the origin. Good. You've got it!

Today, you're given a list of numbers:
--}

exDir, nums :: FilePath
exDir = "Y2018/M12/D19/"
nums = "nums.txt"

{--
But you notice the numbers are not ... quite! ... in the correct format. The
correct format is:

+<Int> for a positive number, and
-<Int> for a negative number

So, for example, you have:

4887
4127
-6400
9231
-2918
...

but what you need is

+4887
+4127
-6400
+9231
-2918
...

read in the above file then output the file in the correct format.
--}

correctNumberFormat :: FilePath -> FilePath -> IO ()
correctNumberFormat inputfile outputFile = undefined

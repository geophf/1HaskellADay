module Y2018.M10.D05.Exercise where

{--
Hello, welcome (back) to 1HaskellADay daily problem-to-solve!

Today's Haskell problem is very file-y.

You have a file:
--}

exDir, files :: FilePath
files = "files.txt"
exDir = "Y2018/M10/D05/"

{--
This file lists a set of files that need to be processed. However, your AWS
lambda function can only process these files at most in five minutes, and these
files are very large and filled with 'stuff.'

So, we need to chunk these files into subsets. How we do that is not today's
exercise. Today's exercise is the work allocation. So, for today, we are going
to write a function that takes a file name and returns a set of SQL INSERT
statements that indicates which chunk we are processing. This is from
the SQL table 'file' (png attached and replicated here):

file table
==========

id, serial
filename, text
step, integer
tot, integer

So, the function

chunk "abc.json" 3

will produce three SQL INSERT statements of the form:

INSERT INTO file (filename,step,tot) VALUES ('abc.json',1,3);
INSERT INTO file (filename,step,tot) VALUES ('abc.json',2,3);
INSERT INTO file (filename,step,tot) VALUES ('abc.json',3,3);
--}

chunk :: FilePath -> Integer -> [String]
chunk filename tot = undefined

-- Also, you need to read in the files to be processed and call chunk to
-- generate the entire set of INSERT statements, thereby creating your workflow

chunkFiles :: FilePath -> Integer -> IO [String]
chunkFiles fileOfFileNames tot = undefined

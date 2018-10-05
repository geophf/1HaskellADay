module Y2018.M10.D05.Solution where

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
chunk filename tot = map (insertStmt filename tot) [1..tot]

insertStmt :: FilePath -> Integer -> Integer -> String
insertStmt file tot step =
   "INSERT INTO file (filename,step,tot) VALUES ('" ++ file ++ "'," 
      ++ show step ++ "," ++ show tot ++ ");"

-- Also, you need to read in the files to be processed and call chunk to
-- generate the entire set of INSERT statements, thereby creating your workflow

chunkFiles :: FilePath -> Integer -> IO [String]
chunkFiles fileOfFileNames tot = readFile fileOfFileNames >>=
   return . concatMap (\file -> chunk file tot) . lines

{--
>>> chunkFiles (exDir ++ files) 2 >>= mapM_ putStrLn . take 5
INSERT INTO file (filename,step,tot) VALUES ('CuPwmHGZmYUr.json',1,2);
INSERT INTO file (filename,step,tot) VALUES ('CuPwmHGZmYUr.json',2,2);
INSERT INTO file (filename,step,tot) VALUES ('MrVGUyN9Vmzq.json',1,2);
INSERT INTO file (filename,step,tot) VALUES ('MrVGUyN9Vmzq.json',2,2);
INSERT INTO file (filename,step,tot) VALUES ('vapQmgqM3red.json',1,2);
--}

module Y2020.M10.D23.Exercise where

{--
So, I managed to update country-names in the graph yesterday, and then I
went to upload the air bases, but got the following error [snippet]:

\"errors\":[{\"code\":\"Neo.ClientError.Statement.SyntaxError\",
\"message\":\"Invalid input '2': expected '\\\\', ''', '\\\"', 'b', 'f', 'n', 
 'r', 't', UTF16 or UTF32 (line 1, column 73 (offset: 72))\\n\\
 \"MERGE (a:Base { url: \\\"http://www.wikidata.org/entity/Q43363\\\",
  name: \\\"Chi\\\\232vres Air Base\\\",icao: \\\"EBCV\\\"

This (hot mess) tells me that wikidate is doing some encoding into its JSON
output, changing unicode characters to \232, for example (e-grave, in this
case).

So, one more step, then: let's find all cases of base-names that have
escaped unicode, and return those bases.

THEN: Let's replace the escaped values with unicode. That shouldn't be
hard, right? It is the 21st century, after all, right?

Actually, ... looking at the raw data, this solution may be easier than
I anticipated.
--}



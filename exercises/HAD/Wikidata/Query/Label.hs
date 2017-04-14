{-# LANGUAGE OverloadedStrings #-}

module Wikidata.Query.Label where

import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Network.HTTP.Conduit

{-- A solution to the problem posted at http://lpaste.net/6471988009620209664
@1HaskellADay solution for 2016-03-01

Okay, revision 3 of today's problem.

I just wanna do this one simple thing, but okay.

So, how do you get the States of the United States from wikidata?

Going to the wikidata REST endpoint and entering a SPARQL query gets me three
MEGABYTES of RDF.

* First of all, I hate RDF.
* Second of all, none of the RDF triples had any of the State labels (names)
* Third of all, none of the RDF triples had any of the State wikidata Q-ids.

Just. Great.

So, I go here:

https://wdq.wmflabs.org/api?q=claim[31:35657]

Machine code, yes, but let's extract from it the original SPARQL query:

SELECT ?state ?stateLabel 
WHERE 
{
	?state wdt:P31 wd:Q35657 .
	SERVICE wikibase:label {
		bd:serviceParam wikibase:language "en" .
	}
}

So you now see the origin of the 31:35657? No? Well those are wikidata 
values that indicate property 31 ('is a member of') and q (whatever that 
means) 35657 ('State of the United States of America').

Now, how you are supposed to know that, I don't know.

But, now you do, and the world is all tea and crumpets again. Yay.

You should see the underlying machine code that created this API.

machine code, n: C++ templated classes containing all deeply nested 
if-statements.

*shudder*

Now the reason why I know this: this man behind the curtain, is that this API
is HELLA slow?!? for C++?!? I mean, like: really?!? But whatever, it's a working
API, which I cannot say for the wikidata REST endpoint (3 megabytes of useless
RDF. Just. great), and it returns ...

... wait for it ...

wikidata identifiers.

What do those identifiers mean?

HELL if I know.

Gosh. Just 

One.
Simple.
Thing.

States of the United States of America

And it's taken me three revisions of simplifying an original query

"Give me the lat/longs of particular cities of the U.S.A. States"

To a simpler query against the wikidata REST endpoint:

"Just give me the States of the United States of America"

to finally throwing up my hands and using a third-party hella-slow tool that,
well, at least works ... halfway. Given that you can program in machine code.

I mean, can't wikidata provide a REST endpoint that returns the results of
a query in JSON? that I can see right there in my browser? But no.

Fine.

Okay, so now that you went to this machine code service:

https://wdq.wmflabs.org/api?q=claim[31:35657]

and gotten your query results:

{"status":
 {"error":"OK",   -- error = "OK" ... great error code. Just. great.
  "items":50,
  "querytime":"2745ms",
  "parsed_query":"CLAIM[31:35657]"
 },"items":[99,173,724,759,...]}

or, more sanely, because, I swear ...
--}

type QID = Int
type USState = String

states :: [QID]
states = [99,173,724,759,771,779,782,797,812,816,824,829,1166,1204,1207,1211,
          1212,1214,1221,1223,1227,1261,1370,1371,1384,1387,1391,1393,1397,
          1400,1408,1415,1428,1439,1454,1456,1494,1509,1522,1527,1537,1546,
          1553,1558,1581,1588,1603,1612,1649,16551]

{--
NOW that you've done and got ALL THAT!

Now today's Haskell problem.

The above machine code result is very pretty, I'm sure. But I'd like to know
the names of the States that these Q-item ids signify? You know, for giggles.

Wikidata does actually provide an endpoint for this very thing, that is:

What is the label for this Q-item? It's called:

https://www.wikidata.org/w/api.php?format=json&action=wbgetentities&props=labels&ids=Q99&languages=en

(Well, it's called that for Q-id 99, anyway)

So, do me a favor, please. Write a REST client that coverts the above States-
as-Ints into States-as-Strings.

*mutter-mutter* 
useless RDF REST endpoint, have to do everything by hand now
*mutter-mutter*
--}

endpoint :: String
endpoint = "https://www.wikidata.org/w/api.php?format=json&action=wbgetentities&props=labels&languages=en&ids="

stateName :: QID -> IO ByteString
stateName = simpleHttp . (endpoint ++) . qname

type QName = String -- a 'name' (id number) that starts with 'Q' because reasons

qname :: Show a => a -> QName
qname = ('Q':) . show

-- please show a list of (QID, JSON containing USState). Thanks.

-- if you don't have Network.HTTP.Conduit it's http-conduit via cabal

-- WARNING: READ THIS!

-- You get back a bunch of JSON. That's fine for today's work. Just get back
-- the JSON, tomorrow we'll parse out the state name from that JSON. Today,
-- just get it back. Baby steps.

-- *Main> stateName 99 ~> "{\"entities\":{\"Q99\":{\"type\":\"item\",
--     \"id\":\"Q99\",\"labels\":{\"en\":{\"language\":\"en\",
--     \"value\":\"California\"}}}},\"success\":1}"

{-- 
So, ya know:
*Main> mapM stateName states
... and wait a while. In fact, a long while, timing this:

*Main Data.Time.Clock Control.Monad> getCurrentTime >>= \start ->
    mapM_ (stateName >=> print) states >> getCurrentTime >>= 
    print . flip diffUTCTime start

gets us a runoff of State JSON results after 18.421259s

Ouch.

Let's better this with just one call to the endpoint:
--}

statesNames :: [QID] -> IO ByteString
statesNames = simpleHttp . (endpoint ++) . intercalate "|" . map qname

{--
Timing this definition:

*Main Data.Time.Clock> getCurrentTime >>= \start ->
   statesNames states >>= print >> getCurrentTime >>=
   print . flip diffUTCTime start

gets the JSON back in 1.08088s, or almost 20x faster! WOOT!
--}

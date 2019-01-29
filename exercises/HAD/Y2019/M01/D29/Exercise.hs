module Y2019.M01.D29.Exercise where

{--
Yesterday we parsed in a bunch of articles and simply admired their contents.

Today, let's do something about them. We have a set of articles, some stored
'simply' (there's that word, again) as HTML, and the others stored as strings
that look 'kinda' like JSON.

Our job is to convert the 'kinda like JSON' strings in pilot-not-json.json
into HTML strings.
--}

import Y2019.M01.D28.Exercise

convert2HTML :: Article -> String
convert2HTML art = undefined

-- convert2HTML takes an article and returns the HTML string of the text.
-- If it's already HTML, it just returns that string, if it's in the JSON-like
-- format it converts that JSON-like string to an HTML string.

-- be careful, some of the JSON-like strings are invalid, so correct the
-- invalidity first before parsing those strings.

-- Use the work you did yesterday to load the articles for processing here.

<?php

// adds articles then shows the source and recommended article set as JSON 
// summaries

$q = $_REQUEST["q"];

exec("adder $q");

$arts = explode(" ",$q);
$artId = $arts[0];

exec("start $artId", $t);

foreach($t as $line) {
  echo $line;
}
?>

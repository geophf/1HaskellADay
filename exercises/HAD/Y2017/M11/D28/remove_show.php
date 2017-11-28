<?php

// removes recommended articles then shows the source and recommended article 
// set as JSON summaries

$q = $_REQUEST["q"];

exec("remover $q");

$arts = explode(" ",$q);
$artId = $arts[0];

exec("start $artId", $t);

foreach($t as $line) {
  echo $line;
}
?>

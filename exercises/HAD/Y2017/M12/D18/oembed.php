<?php

// Returns the JSON from an oembed request

$form = $_REQUEST["format"];
$url = $_REQUEST["url"];

exec("oeResp $form $url", $t);

foreach($t as $line) {
  echo $line;
}
?>

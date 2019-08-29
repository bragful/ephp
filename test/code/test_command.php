<?php

$filter = 'ephp';
var_dump(trim(`cat src/ephp.app.src | grep $filter | head -1`));
$result = trim(`echo \``);
var_dump(strpos($result, "sh:") !== FALSE);
var_dump(strpos($result, "yntax error") !== FALSE);

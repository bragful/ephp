<?php

$f = fopen("asc:data", "w+");
var_dump($f);
fclose($f);

if (file_exists("asc:data")) print "OK\n";


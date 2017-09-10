<?php

$f = fopen("/tmp/data", "w");
fwrite($f, "hello world");
fclose($f);

$f = fopen("/tmp/data", "r");
while (!feof($f)) {
    $c = fread($f, 1);
    print "[$c]";
}
print "\n";
var_dump(feof($f));
fclose($f);


<?php
$mystring = 'abc';
$findme   = 'a';
$pos = strpos($mystring, $findme);

if ($pos === false) {
    echo "Not found '$findme' in '$mystring'\n";
} else {
    echo "Found '$findme' in '$mystring' and pos $pos\n";
}

$newstring = 'abcdef abcdef';
$pos = strpos($newstring, 'a', 1);
var_dump($pos);

strpos(array(), 'a');
strpos('a', array());
strpos('a', 'a', 'a');
strpos('a', 'a', 10);

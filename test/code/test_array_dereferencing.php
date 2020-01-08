<?php
function getArray() {
    return array(1, 2, 3);
}

// on PHP 5.4
$secondElement = getArray()[1];
var_dump($secondElement);

// previously
$tmp = getArray();
$secondElement = $tmp[1];
var_dump($secondElement);

// or
list(, $secondElement) = getArray();
var_dump($secondElement);

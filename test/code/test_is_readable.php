<?php

$filename = __FILE__;
if (is_readable($filename)) {
    echo basename(__FILE__) . ": This file is readable\n";
} else {
    echo basename(__FILE__) . ": This file is not readable\n";
}

$filename = "/dev/file_not_found";
if (is_readable($filename)) {
    echo "$filename: This file is readable\n";
} else {
    echo "$filename: This file is not readable\n";
}

var_dump(is_readable(array()));
var_dump(is_readable(new stdClass()));


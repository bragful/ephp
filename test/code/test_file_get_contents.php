<?php

$data = file_get_contents(__FILE__);
$parts = explode("\n", $data);
print $parts[2] . "\n";

<?php

$f = fopen("/tmp/test_stream_fseek", "w+");
var_dump(fwrite($f, "hello world!\n"));
var_dump(fseek($f, 0));
var_dump(fwrite($f, "Hallo"));
var_dump(fseek($f, 0));
$data = fread($f, 20);
var_dump($data);
fclose($f);


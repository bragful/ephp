<?php

$data = "\x20\x30\x31\x20\x30\x31";
var_dump($data);
$data = '\x20\x30\x31\x20\x30\x31';
var_dump($data);

$data = "\040\060\061\040\060\061";
var_dump($data);
$data = '\040\060\061\040\060\061';
var_dump($data);

var_dump("data\0\x\40\xdhey!\xD");

<?php

$data = array(1,2,3,4);
$other = array('a','b','c',&$data);
$data[] = &$other;

var_dump($data);


<?php

class Data {
    public function &data() {
        $a = new stdClass;
        $a->hi = "hello world!";
        return $a;
    }
}

$d = new Data;
$b = &$d->data();
var_dump($b);

$c = &Data::data();
var_dump($c);


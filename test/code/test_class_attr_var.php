<?php

class Hello {
    public $data = "hola";
}

$h = new Hello();
$attr = "data";
var_dump($h->$attr);


<?php

class Simple {
    public $data = array();

    public function __construct() {
        $this->data = array(
            "hi" => "world"
        );
    }
}

$simple = new Simple();
var_dump($simple->data);
var_dump($simple->data['hi']);


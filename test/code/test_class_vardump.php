<?php

class User {
    public $name = "John";
    public $surname = "Smith";
}

$user = new User();
var_dump($user);

print_r($user);

print "type => " . gettype($user) . "\n";


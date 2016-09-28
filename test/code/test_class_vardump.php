<?php

class User {
    public $name = "John";
    public $surname = "Smith";
}

$user = new User();
var_dump($user);

print_r($user);
print "print_r => " . print_r($user, true) . "\n";

print "type => " . gettype($user) . "\n";


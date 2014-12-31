<?php

class User {
    public $name = "No";
    public $surname = "Defined";

    public function __construct() {
        $this->name = "John";
        $this->surname = "Smith";
    }

    public function toString() {
        return "User<" . $this->name . " " . $this->surname . ">";
    }
}

$user = new User();
print $user->toString() . "\n";


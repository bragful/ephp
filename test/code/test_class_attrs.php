<?php

class Person {
    private $name;
    private $surname;

    public function __construct($name, $surname) {
        $this->name = $name;
        $this->surname = $surname;
    }

    public function getName() { return $this->name; }
    public function getSurname() { return $this->surname; }

    public function __toString() {
        return "My name is {$this->surname}... {$this->name} {$this->surname}\n";
    }
}

$p = new Person("Manuel", "Rubio");
print $p;
print $p->name;
print $p->surname;
print "OK\n";

<?php

class A {
    private $a = 100;
    public $b = 100;
    protected $c = 100;

    public function get_a() {
        $this->a ++;
        return $this->a;
    }

    public function get_b() {
        $this->b ++;
        return $this->b;
    }

    public function get_c() {
        $this->c ++;
        return $this->c;
    }
}

class B extends A {
    public $a = 200;
    public $b = 200;
    public $c = 200;

    public function get_my_a() {
        return $this->a;
    }

    public function get_my_b() {
        return $this->b;
    }

    public function get_my_c() {
        return $this->c;
    }
}

$b = new B;

print "\nprivate example of override 'a'...\n";
print "parent a => " . $b->get_a() . "\n";
print "child  a => " . $b->get_my_a() . "\n";
$b->a = 50;

print "parent a => " . $b->get_a() . "\n";
print "child  a => " . $b->get_my_a() . "\n";

print "\npublic example of override 'b'...\n";
print "parent b => " . $b->get_b() . "\n";
print "child  b => " . $b->get_my_b() . "\n";
$b->b = 50;

print "parent b => " . $b->get_b() . "\n";
print "child  b => " . $b->get_my_b() . "\n";

print "\nprotected example of override 'c'...\n";
print "parent c => " . $b->get_c() . "\n";
print "child  c => " . $b->get_my_c() . "\n";
$b->c = 50;

print "parent c => " . $b->get_c() . "\n";
print "child  c => " . $b->get_my_c() . "\n";


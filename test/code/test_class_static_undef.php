<?php

class A {
    static $a;

    static function data() {
        static $b;
        if (!isset(self::$a)) {
            print __CLASS__ . " init \$a = 10\n";
            self::$a = 10;
        } else {
            self::$a += 10;
            print __CLASS__ . " update \$a to " . self::$a . "\n";
        }

        if (!isset($b)) {
            print __CLASS__ . " init \$b = 10\n";
            $b = 10;
        } else {
            $b += 10;
            print __CLASS__ . " update \$b to $b\n";
        }
    }
}

class B extends A {}

A::data();
print "---\n";
A::data();
print "---\n";
A::data();
print "===\n";
B::data();
print "---\n";
B::data();
print "---\n";
B::data();


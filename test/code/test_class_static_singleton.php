<?php

class OnlyOne {
    private static $singleton;

    private function __construct() {
        print "creating " . __CLASS__ . "\n";
    }
    static public function get() {
        if (!self::$singleton) {
            self::$singleton = new OnlyOne();
        }
        return self::$singleton;
    }
}

$a = OnlyOne::get();
$b = OnlyOne::get();
var_dump($a, $b);

new OnlyOne();

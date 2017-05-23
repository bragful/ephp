<?php

class Hello {
    public function greetings() {
        static $a = 10, $b = 20;
        print "($a) ($b) hello world\n";
        $a += 10;
        $b *= 2;
    }

    public static function world() {
        static $calls = 0;
        $calls ++;
        print "calling to hello world $calls\n";
    }
}

Hello::world();
Hello::world();
Hello::world();

$hello = new Hello;
$hello->greetings();
$hello->greetings();
$hello->greetings();

<?php

class Hello {

    public static $greeting = 'hello';

    public static function hello_world() {
        print self::$greeting . " world!\n";
    }

}

Hello::hello_world();


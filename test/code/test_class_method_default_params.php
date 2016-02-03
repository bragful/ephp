<?php

class Hello {

    public function show($greeting = 'hello', $who = 'world!') {
        print "$greeting $who!\n";
    }

}

$hello = new Hello();
$hello->show('ciao', 'mondo');
$hello->show('hei');
$hello->show();

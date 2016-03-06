<?php

class A {

    private function no_call_me() {
        print "I told you!\n";
    }

}

$a = new A();
$a->no_call_me();


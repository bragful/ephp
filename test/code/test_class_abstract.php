<?php

date_default_timezone_set('UTC');

abstract class Abstracta {
    abstract public function get_time();
    abstract public function get_date();
}

class Specifica extends Abstracta {
    public function get_time() {
        return date("H:i:s", 1505537466);
    }

    public function get_date() {
        return date("Y-m-d", 1505537466);
    }
}

$s = new Specifica;
print $s->get_date() . " " . $s->get_time() . "\n";


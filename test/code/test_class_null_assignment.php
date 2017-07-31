<?php

class Simple {
    public function __construct() { print "construct!\n"; }
    public function __destruct() { print "destruct!\n"; }
}

$a = new Simple;
$b = $a;

?>
Unset $a
<? $a = NULL; ?>
Unset $b
<? $b = NULL; ?>
END


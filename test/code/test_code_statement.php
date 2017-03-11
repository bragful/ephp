<?php

function is_greet($greet) {
    $greetings = [ "hi", "hello" ];
    foreach ($greetings as $i)
        if ($greet == $i) {
            print "yes! $greet is a greet!\n";
        }
}

is_greet("hi");
is_greet("bye");

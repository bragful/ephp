<?php

$real_options = [ "hi" => "Hello %s!" ];

function greeting($who, $options = '') {
    if ( $options == '' )
        global $real_options;
    else
        $real_options = $options;

    printf($real_options['hi'] . "\n", $who);
}

greeting("world");
greeting("mundo", [ 'hi' => 'Hola %s!' ]);


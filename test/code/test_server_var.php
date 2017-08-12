<?php

function data() {
    if (!isset($_SERVER["DATA"])) $_SERVER["DATA"] = 10;
    $default_server_values = array(
        'SERVER_SOFTWARE' => '',
        'REQUEST_URI' => '',
    );
    $_SERVER = array_merge( $default_server_values, $_SERVER );
}

$_SERVER['REQUEST_URI'] = '/server';
data();
data();
var_dump([$_SERVER['REQUEST_URI'], $_SERVER['DATA']]);


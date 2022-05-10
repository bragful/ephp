<?php

$data = [
    'sanitize_callback' => function( $value ) {
        return 'excerpt' === $value || 'full' === $value ? $value : 'excerpt';
    },
];

var_dump($data['sanitize_callback']('excerpt'));
var_dump($data['sanitize_callback']('full'));
var_dump($data['sanitize_callback']('other'));

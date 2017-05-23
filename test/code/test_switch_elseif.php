<?php

$token = 'a';

switch ( $token ) {
    case 's' :
        if ( $strict ) {
            if ( '"' !== substr( $data, -2, 1 ) ) {
                return false;
            }
        } elseif ( !$strict ) {
        }
    case 'a' :
    case 'O' :
        $data = (bool) 100;
}

var_dump($data);

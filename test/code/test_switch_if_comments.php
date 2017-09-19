<?php

$mode = 0;

switch ($mode) {
    case 0; // here a comment 

        if ( 1 == 1 ) // another
            {
                print "(1/3) OK\n";
            }
        if ( 2 == 2 ) # and other
            { print "(2/3) OK\n"; }
        if ( 3 == 3 ) /* and a final one */
            { print "(3/3) OK\n"; }
        break;

    case 1 :

        break;
}


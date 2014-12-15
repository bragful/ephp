<?php

for ($i=0; $i<5; $i++) {
    switch ($i) {
        case 4:
            print "4 is greater than ";
        case 2:
            print "2 is greater than ";
        case 0:
            print "0... even numbers\n";
            break;
        default:
            print "$i is odd\n";
    }
}


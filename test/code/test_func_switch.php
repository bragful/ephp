<?php

function translate_level_to_role($level) {
        switch ($level) {
        case 10:
        case 9:
        case 8:
                return 'administrator';
        case 7:
        case 6:
        case 5:
                return 'editor';
        case 4:
        case 3:
        case 2:
                return 'author';
        case 1:
                return 'contributor';
        case 0:
                return 'subscriber';
        }
}

for ($i=0; $i<10; $i++)
    print "$i -> " . translate_level_to_role($i) . "\n";


<?php

if (@file_exists("a.a") and @file_exists("b.b")) {
    print "NO!\n";
} else {
    print "OK\n";
}


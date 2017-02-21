<?php

if (!file_exists("this_file_doesnt_exist")) {
    print "perfect! file doesn't exist\n";
}

if (file_exists(__FILE__)) {
    print "yes! this file " . __FILE__ . " exists!\n";
}

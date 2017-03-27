<?php
$a = array('1.10', 12.4, 1.13);

if (in_array('12.4', $a, true))
    print "Found '12.4' strict\n";

if (in_array('12.4', $a, false))
    print "Found 12.4 no strict\n";

if (in_array(1.13, $a, true))
    echo "Found 1.13 strict\n";

var_dump(in_array(1, $a, 25));
var_dump(in_array(1, $a, array()));

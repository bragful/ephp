<?php
$data = "Two Ts and one F.";

for ($i=0; $i<=4; $i++) {
    print "Mode: $i\n";
    var_dump(count_chars($data, $i));
    print "End mode $i\n";
}

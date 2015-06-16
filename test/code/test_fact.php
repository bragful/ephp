<?

function fact($a) {
    if ($a == 1) {
        return 1;
    }
    return $a * fact($a - 1);
}

print fact(90) . "\n";


<?php

namespace Coding\Style\MyCode;

function hello($world) {
    print __FUNCTION__ . " is saying: hello $world!\n";
}

print MissingConst . "\n";
hello("world");

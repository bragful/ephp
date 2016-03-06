<?php

function show($greeting='hello', $who='world') {
    print $greeting . " " . $who . "\n";
}

print show("Hi", "mum!");
print show("Hello");
print show();


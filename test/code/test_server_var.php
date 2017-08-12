<?php

function data() {
    if (!isset($_SERVER["DATA"])) $_SERVER["DATA"] = 10;
}

data();
print $_SERVER["DATA"] . "\n";


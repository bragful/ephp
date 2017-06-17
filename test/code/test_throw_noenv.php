<?php

function crash(&$name) {
    throw new Exception;
}

function call_to_crash() {
    $name = "world's name";
    crash($name);
}

call_to_crash();


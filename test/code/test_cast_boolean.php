<?php

function data($data) {
    return (boolean) $data;
}

var_dump([
    data(1),
    data(0)
]);


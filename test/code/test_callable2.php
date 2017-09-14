<?php

//$result = $a[PCLZIP_CB_PRE_ADD](PCLZIP_CB_PRE_ADD, $data);

$a = [ "print_r", "var_dump" ];

const PRINT_R = 0;
const VAR_DUMP = 1;

$data = [1,2,3];

$result = $a[PRINT_R]($data);
$result = $a[VAR_DUMP](PCLZIP_CB_PRE_ADD, $data);


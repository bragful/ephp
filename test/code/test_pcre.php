<?php
$sujeto = "abcdef";
$patrón = '/def/';
preg_match($patrón, $sujeto, $coincidencias, PREG_OFFSET_CAPTURE);
print_r($coincidencias);

var_dump(preg_match($patrón));

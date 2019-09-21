<?php
$foo = "0123456789a123456789b123456789c";

// Looking for '0' from the 0th byte (from the beginning)
var_dump(strrpos($foo, '0', 0));

// Looking for '0' from the 1st byte (after byte "0")
var_dump(strrpos($foo, '0', 1));

// Looking for '7' from the 21th byte (after byte 20)
var_dump(strrpos($foo, '7', 20));

// Looking for '7' from the 29th byte (after byte 28)
var_dump(strrpos($foo, '7', 28));

// Looking for '7' right to left from the 5th byte from the end
var_dump(strrpos($foo, '7', -5));

// Looking for 'c' right to left from the 2nd byte from the end
var_dump(strrpos($foo, 'c', -2));

// Looking for '9c' right to left from the 2nd byte from the end
var_dump(strrpos($foo, '9c', -2));

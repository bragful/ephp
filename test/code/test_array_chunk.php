<?php

$input_array = array('a', 'b', 'c', 'd', 'e');
print_r(array_chunk($input_array, 2));
print_r(array_chunk($input_array, 2, true));

$input_array = array('a', 'b', 'c', 'd');
print_r(array_chunk($input_array, 2));
print_r(array_chunk($input_array, 2, true));

print_r(array_chunk($input_array, 5));
print_r(array_chunk($input_array, 5, true));

print_r(array_chunk(array(), 10));
print_r(array_chunk(array(), 10, true));

var_dump(array_chunk("hello!", 5));


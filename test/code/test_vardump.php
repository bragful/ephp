<?php

# This test differs from PHP to ePHP because the
# loop detection is done in a different way.
# Anyway, I want to keep this test to ensure
# the loop detection is working properly.

$data = [array(1,2,3,4)];
$other = array('a','b','c',&$data);
$data[0][] = &$other;

var_dump($data);


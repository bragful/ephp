<?php

function data() {
	print basename(__FILE__) . "\n";
	print basename(__DIR__) . "\n";
	print __LINE__ . "\n";
	print __FUNCTION__ . "\n";
}

data();
print __FUNCTION__ . "\n";


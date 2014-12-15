<?php

$message = "----";

function data($who) {
	global $message;
	print "inside: $who\n";
	$message = "ref modify = $who\n";
	return "return: --$who--\n";
}

print data("World");
print $message . "\n";


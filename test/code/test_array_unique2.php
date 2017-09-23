<?php
$list = array(
  '0',
  null,
  0,
  10,
  '10.0',
  '1e1',
  false,
  ''
);
print "------\nDEFAULT\n------\n";
var_dump(array_unique($list));
print "------\nSTRING\n------\n";
var_dump(array_unique($list, SORT_STRING));
print "------\nNUMERIC\n------\n";
var_dump(array_unique($list, SORT_NUMERIC));
print "------\nREGULAR\n------\n";
var_dump(array_unique($list, SORT_REGULAR));


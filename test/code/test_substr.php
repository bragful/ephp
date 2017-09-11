<?php

print "substr([], 0);\n";
var_dump(substr([], 0));

print "substr(\"abcdef\", 0, 0);\n";
var_dump(substr("abcdef", 0, 0));

print "substr(\"abcdef\", -1);\n";
var_dump(substr("abcdef", -1));
print "substr(\"abcdef\", -2);\n";
var_dump(substr("abcdef", -2));
print "substr(\"abcdef\", -7);\n";
var_dump(substr("abcdef", -7));
print "substr(\"abcdef\", -3, 1);\n";
var_dump(substr("abcdef", -3, 1));
print "substr(\"abcdef\", -3, 3);\n";
var_dump(substr("abcdef", -3, 3));
print "substr(\"abcdef\", -3, 4);\n";
var_dump(substr("abcdef", -3, 4));
print "substr(\"abcdef\", -3, -1);\n";
var_dump(substr("abcdef", -3, -1));
print "substr(\"abcdef\", -3, -3);\n";
var_dump(substr("abcdef", -3, -3));
print "substr(\"abcdef\", -3, -4);\n";
var_dump(substr("abcdef", -3, -4));

print "substr(\"abcdef\", 0);\n";
var_dump(substr("abcdef", 0));
print "substr(\"abcdef\", 1);\n";
var_dump(substr("abcdef", 1));
print "substr(\"abcdef\", 6);\n";
var_dump(substr("abcdef", 6));
print "substr(\"abcdef\", 3, 1);\n";
var_dump(substr("abcdef", 3, 1));
print "substr(\"abcdef\", 3, 3);\n";
var_dump(substr("abcdef", 3, 3));
print "substr(\"abcdef\", 3, 4);\n";
var_dump(substr("abcdef", 3, 4));
print "substr(\"abcdef\", 3, -1);\n";
var_dump(substr("abcdef", 3, -1));
print "substr(\"abcdef\", 3, -3);\n";
var_dump(substr("abcdef", 3, -3));
print "substr(\"abcdef\", 3, -4);\n";
var_dump(substr("abcdef", 3, -4));

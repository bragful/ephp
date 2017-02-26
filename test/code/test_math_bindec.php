<?php

class A {}

print bindec("101") . "\n";
print bindec("150") . "\n";
print bindec("aa") . "\n";
print bindec(array()) . "\n";
print bindec(100) . "\n";
print bindec(new A()) . "\n";
print "end\n";

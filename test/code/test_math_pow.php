<?php

class A {}

print "pow(10,2) = " . pow(10, 2) . "\n";
print "pow(10,'2') = " . pow(10, "2") . "\n";
print "pow('a',2) = " . pow('a', 2) . "\n";
print "pow(10,'a') = " . pow(10, 'a') . "\n";
print "pow(array(),2) = " . pow(array(),2) . "\n";
print "pow(new A(), 2) = " . pow(new A(), 2) . "\n";
print "pow(10, new A()) = " . pow(10, new A()) . "\n";
print "pow(new A(), new A()) = " . pow(new A(), new A()) . "\n";

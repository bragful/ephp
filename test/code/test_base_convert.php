<?php

class A {}

print "base_convert(101, 2, 10) = " . base_convert(101, 2, 10) . "\n";
print "base_convert(101, 2, '') = " . base_convert(101, 2, '') . "\n";
print "base_convert(101, '', 10) = " . base_convert(101, '', 10) . "\n";
print "base_convert(101, 1, 1) = " . base_convert(101, 1, 1) . "\n";
print "base_convert(101, 2, 1) = " . base_convert(101, 2, 1) . "\n";
print "base_convert('13031', 2, 10) = " . base_convert('13031', 2, 10) . "\n";
print "base_convert('AB10', 16, 10) = " . base_convert('AB10', 16, 10) . "\n";
print "base_convert(array(), 2, 2) = " . base_convert(array(), 2, 2) . "\n";
print "base_convert(new A, 2, 2) = " . base_convert(new A, 2, 2) . "\n";
print "end\n";

<?php

var_dump(pack("a20", 21));
var_dump(pack("a4", 12345678));
var_dump(pack("A20", 21));
var_dump(pack("A4", 12345678));
var_dump(pack("a2", 20));
var_dump(pack("A2", 20));

var_dump(bin2hex(pack("H*", "AB")));
var_dump(bin2hex(pack("H2", "2C")));
var_dump(bin2hex(pack("H1", "A9")));
var_dump(bin2hex(pack("H3", "A9B")));

var_dump(bin2hex(pack("h*", "AB")));
var_dump(bin2hex(pack("h2", "2C")));
var_dump(bin2hex(pack("h1", "A9")));
var_dump(bin2hex(pack("h3", "A9B")));

var_dump(bin2hex(pack("c", 33)));
var_dump(bin2hex(pack("c", "")));
var_dump(pack("c*", 33, 34, 35));
var_dump(pack("c2", 33, 34));
var_dump(pack("c2", 33, 34, 35));
var_dump(pack("c3", 33));

print "integers...\n";
var_dump(bin2hex(pack("s", 65123)));
var_dump(bin2hex(pack("s*", 6512, 1234)));
var_dump(bin2hex(pack("s2", 5678, 3212)));
 
var_dump(bin2hex(pack("S", 0x4142)));
var_dump(bin2hex(pack("S*", 0x1234, 0x123, 0x5678)));
var_dump(bin2hex(pack("S2", 0x123, 0x456)));
var_dump(bin2hex(pack("S", -100)));

var_dump(bin2hex(pack("v", 65123)));
var_dump(bin2hex(pack("v*", 6512, 1234)));
var_dump(bin2hex(pack("v2", 5678, 3212)));

var_dump(bin2hex(pack("n", 65123)));
var_dump(bin2hex(pack("n*", 6512, 1234)));
var_dump(bin2hex(pack("n2", 5678, 3212)));

var_dump(bin2hex(pack("i", 65123)));
var_dump(bin2hex(pack("i*", 6512, 1234)));
var_dump(bin2hex(pack("i2", 5678, 3212)));

var_dump(bin2hex(pack("l", 65123)));
var_dump(bin2hex(pack("l*", 6512, 1234)));
var_dump(bin2hex(pack("l2", 5678, 3212)));

var_dump(bin2hex(pack("I", 65123)));
var_dump(bin2hex(pack("I*", 6512, 1234)));
var_dump(bin2hex(pack("I2", 5678, 3212)));

var_dump(bin2hex(pack("L", 65123)));
var_dump(bin2hex(pack("L*", 6512, 1234)));
var_dump(bin2hex(pack("L2", 5678, 3212)));

var_dump(bin2hex(pack("N", 65123)));
var_dump(bin2hex(pack("N*", 6512, 1234)));
var_dump(bin2hex(pack("N2", 5678, 3212)));

var_dump(bin2hex(pack("V", 65123)));
var_dump(bin2hex(pack("V*", 6512, 1234)));
var_dump(bin2hex(pack("V2", 5678, 3212)));

var_dump(bin2hex(pack("q", 65123)));
var_dump(bin2hex(pack("q*", 6512, 1234)));
var_dump(bin2hex(pack("q2", 5678, 3212)));

var_dump(bin2hex(pack("Q", 65123)));
var_dump(bin2hex(pack("Q*", 6512, 1234)));
var_dump(bin2hex(pack("Q2", 5678, 3212)));

var_dump(bin2hex(pack("J", 65123)));
var_dump(bin2hex(pack("J*", 6512, 1234)));
var_dump(bin2hex(pack("J2", 5678, 3212)));

var_dump(bin2hex(pack("P", 65123)));
var_dump(bin2hex(pack("P*", 6512, 1234)));
var_dump(bin2hex(pack("P2", 5678, 3212)));

print "floats...\n";
var_dump(bin2hex(pack("f", 12.0)));
var_dump(bin2hex(pack("f*", 12.0, 23.34)));
var_dump(bin2hex(pack("f2", 12.0, 23,34)));

var_dump(bin2hex(pack("d", 12.0)));
var_dump(bin2hex(pack("d*", 12.0, 23.34)));
var_dump(bin2hex(pack("d2", 12.0, 23,34)));

print "specials...\n";
var_dump(bin2hex(pack("x", 12.0)));
var_dump(bin2hex(pack("nX", 12.0)));
var_dump(bin2hex(pack("nnX", 12, 24)));
var_dump(bin2hex(pack("i2X", 12, 24)));
var_dump(bin2hex(pack("x*", 12.0)));
var_dump(bin2hex(pack("ccX*", 12, 24)));
var_dump(bin2hex(pack("ccX10", 12, 24)));
var_dump(bin2hex(pack("Z", 12.0)));
var_dump(bin2hex(pack("Z*", "hello")));
var_dump(bin2hex(pack("Z1", "hello")));
var_dump(bin2hex(pack("Z4", "hello")));
var_dump(bin2hex(pack("Z0", 12)));
var_dump(bin2hex(pack("Z7", 12)));
var_dump(bin2hex(pack("Z-1", 12)));
var_dump(bin2hex(pack("nZ0n", 12, 12, 12)));
var_dump(bin2hex(pack("nZ0x0", 12, 12)));
var_dump(bin2hex(pack("a01Z0x0", 12, 12)));
var_dump(bin2hex(pack("n@8n", 12, 12)));
var_dump(bin2hex(pack("n@3n", 12, 12)));
var_dump(bin2hex(pack("n@0n", 12, 12)));

print "some errors...\n";
var_dump(bin2hex(pack("D", 12.0)));


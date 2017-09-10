<?php

print "------------\ncmd: a...\n";
var_dump(unpack("a", "hello!"));
var_dump(unpack("a", "hello!\0\0\0"));
var_dump(unpack("a9", "hello!\0\0\0"));
var_dump(unpack("a20", "hello!"));
var_dump(unpack("a*name", "hi world!"));
var_dump(unpack("a2/a3/a1", "ababca"));
var_dump(unpack("a2a/a3b/a1c", "ababca"));

print "------------\ncmd: A...\n";
var_dump(unpack("A", "hello!"));
var_dump(unpack("A", "hello!   "));
var_dump(unpack("A9", "hello!   "));
var_dump(unpack("A20", "hello!"));
var_dump(unpack("A*name", "hi world!"));
var_dump(unpack("A2/A3/A1", "ababca"));
var_dump(unpack("A2a/A3b/A1c", "ababca"));

print "------------\ncmd: h...\n";
var_dump(unpack("h", "\x1a\x2f"));
var_dump(unpack("h3", "\x1a\x2f"));
var_dump(unpack("h20", "\x1a\x2f"));
var_dump(unpack("h*name", "\x1a\x2f\x3c\xdd\x12\x34"));
var_dump(unpack("h2/h3/h1", "\x1a\x2f\xaa\xbb\xcc\x4e"));
var_dump(unpack("h2a/h3b/h1c", "\x1a\x2f\xaa\xbb\xcc\x4e"));

print "------------\ncmd: H...\n";
var_dump(unpack("H", "\x1a\x2f"));
var_dump(unpack("H3", "\x1a\x2f"));
var_dump(unpack("H20", "\x1a\x2f"));
var_dump(unpack("H*name", "\x1a\x2f\x3c\xdd\x12\x34"));
var_dump(unpack("H2/H3/H1", "\x1a\x2f\xaa\xbb\xcc\x4e"));
var_dump(unpack("H2a/H3b/H1c", "\x1a\x2f\xaa\xbb\xcc\x4e"));

print "------------\ncmd: c...\n";
var_dump(unpack("c", "hello world"));
var_dump(unpack("c3", "hello world"));
var_dump(unpack("c20", "hello world"));
var_dump(unpack("c*name", "hello world"));
var_dump(unpack("c2a/c3b/c1c", "hello world"));
var_dump(unpack("c", "\xF0"));
var_dump(unpack("c0", "hello world"));

print "------------\ncmd: C...\n";
var_dump(unpack("C", "hello world"));
var_dump(unpack("C3", "hello world"));
var_dump(unpack("C20", "hello world"));
var_dump(unpack("C*name", "hello world"));
var_dump(unpack("C2a/C3b/C1c", "hello world"));
var_dump(unpack("C", "\xF0"));
var_dump(unpack("C0", "hello world"));

print "------------\ncmd: s...\n";
var_dump(unpack("s", "hello world"));
var_dump(unpack("s3", "hello world"));
var_dump(unpack("s20", "hello world"));
var_dump(unpack("s*name", "hello world"));
var_dump(unpack("s2a/s3b/s1c", "hello world!"));
var_dump(unpack("s0", "hello world"));
var_dump(unpack("s", "\xF0\xF0"));

print "------------\ncmd: S...\n";
var_dump(unpack("S", "hello world"));
var_dump(unpack("S3", "hello world"));
var_dump(unpack("S20", "hello world"));
var_dump(unpack("S*name", "hello world"));
var_dump(unpack("S2a/S3b/S1c", "hello world!"));
var_dump(unpack("S0", "hello world"));
var_dump(unpack("S", "\xF0\xF0"));

print "------------\ncmd: n...\n";
var_dump(unpack("n", "hello world"));
var_dump(unpack("n3", "hello world"));
var_dump(unpack("n20", "hello world"));
var_dump(unpack("n*name", "hello world"));
var_dump(unpack("n2a/n3b/n1c", "hello world!"));
var_dump(unpack("n0", "hello world"));
var_dump(unpack("n", "\xF0\xF0"));

print "------------\ncmd: v...\n";
var_dump(unpack("v", "hello world"));
var_dump(unpack("v3", "hello world"));
var_dump(unpack("v20", "hello world"));
var_dump(unpack("v*name", "hello world"));
var_dump(unpack("v2a/v3b/v1c", "hello world!"));
var_dump(unpack("v0", "hello world"));
var_dump(unpack("v", "\xF0\xF0"));

print "------------\ncmd: i...\n";
var_dump(unpack("i", "hello world!"));
var_dump(unpack("i3", "hello world!"));
var_dump(unpack("i20", "hello world!"));
var_dump(unpack("i*name", "hello world!"));
var_dump(unpack("i2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("i0", "hello world!"));
var_dump(unpack("i", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: I...\n";
var_dump(unpack("I", "hello world!"));
var_dump(unpack("I3", "hello world!"));
var_dump(unpack("I20", "hello world!"));
var_dump(unpack("I*name", "hello world!"));
var_dump(unpack("I2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("I0", "hello world!"));
var_dump(unpack("I", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: l...\n";
var_dump(unpack("l", "hello world!"));
var_dump(unpack("l3", "hello world!"));
var_dump(unpack("l20", "hello world!"));
var_dump(unpack("l*name", "hello world!"));
var_dump(unpack("l2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("l0", "hello world!"));
var_dump(unpack("l", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: L...\n";
var_dump(unpack("L", "hello world!"));
var_dump(unpack("L3", "hello world!"));
var_dump(unpack("L20", "hello world!"));
var_dump(unpack("L*name", "hello world!"));
var_dump(unpack("L2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("L0", "hello world!"));
var_dump(unpack("L", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: V...\n";
var_dump(unpack("V", "hello world!"));
var_dump(unpack("V3", "hello world!"));
var_dump(unpack("V20", "hello world!"));
var_dump(unpack("V*name", "hello world!"));
var_dump(unpack("V2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("V0", "hello world!"));
var_dump(unpack("V", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: N...\n";
var_dump(unpack("N", "hello world!"));
var_dump(unpack("N3", "hello world!"));
var_dump(unpack("N20", "hello world!"));
var_dump(unpack("N*name", "hello world!"));
var_dump(unpack("N2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("N0", "hello world!"));
var_dump(unpack("N", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: q...\n";
var_dump(unpack("q", "hello world!"));
var_dump(unpack("q3", "hello world!hello world!"));
var_dump(unpack("q20", "hello world!hello world!"));
var_dump(unpack("q*name", "hello world!hello world!"));
var_dump(unpack("q2a/v3b/v1c", "hello world!hello world!hello world!hello world!"));
var_dump(unpack("q0", "hello world!"));
var_dump(unpack("q", "\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0"));

print "------------\ncmd: Q...\n";
var_dump(unpack("Q", "hello world!"));
var_dump(unpack("Q3", "hello world!hello world!"));
var_dump(unpack("Q20", "hello world!hello world!"));
var_dump(unpack("Q*name", "hello world!hello world!"));
var_dump(unpack("Q2a/v3b/v1c", "hello world!hello world!hello world!hello world!"));
var_dump(unpack("Q0", "hello world!"));
var_dump(unpack("Q", "\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0"));

print "------------\ncmd: P...\n";
var_dump(unpack("P", "hello world!"));
var_dump(unpack("P3", "hello world!hello world!"));
var_dump(unpack("P20", "hello world!hello world!"));
var_dump(unpack("P*name", "hello world!hello world!"));
var_dump(unpack("P2a/v3b/v1c", "hello world!hello world!hello world!hello world!"));
var_dump(unpack("P0", "hello world!"));
var_dump(unpack("P", "\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0"));

print "------------\ncmd: J...\n";
var_dump(unpack("J", "hello world!"));
var_dump(unpack("J3", "hello world!hello world!"));
var_dump(unpack("J20", "hello world!hello world!"));
var_dump(unpack("J*name", "hello world!hello world!"));
var_dump(unpack("J2a/v3b/v1c", "hello world!hello world!hello world!hello world!"));
var_dump(unpack("J0", "hello world!"));
var_dump(unpack("J", "\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0"));

print "------------\ncmd: f...\n";
var_dump(unpack("f", "hello world!"));
var_dump(unpack("f3", "hello world!"));
var_dump(unpack("f20", "hello world!"));
var_dump(unpack("f*name", "hello world!"));
var_dump(unpack("f2a/v3b/v1c", "hello world!hello world!"));
var_dump(unpack("f0", "hello world!"));
var_dump(unpack("f", "\xF0\xF0\xF0\xF0"));

print "------------\ncmd: d...\n";
var_dump(unpack("d", "hello world!"));
var_dump(unpack("d3", "hello world!hello world!"));
var_dump(unpack("d20", "hello world!hello world!"));
var_dump(unpack("d*name", "hello world!hello world!"));
var_dump(unpack("d2a/v3b/v1c", "hello world!hello world!hello world!hello world!"));
var_dump(unpack("d0", "hello world!"));
var_dump(unpack("d", "\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0"));

var_dump(unpack("xa/cb/cc", "\1\2\3"));
var_dump(unpack("x*", "\1\2"));
var_dump(unpack("x20", "\1\2"));

var_dump(unpack("@3a/cb/cc", "\1\2\3\4\5\6"));
var_dump(unpack("@20", "\1\2\3"));

var_dump(unpack("nA/X2/c2A", "\1\2\3\4\5\6"));
var_dump(unpack("X10", ""));

var_dump(unpack("Z*", "hello\0\0"));
var_dump(unpack("Z6", "hello\0\0"));
var_dump(unpack("Zname", "hello\0\0"));
var_dump(unpack("Z20", "hello"));

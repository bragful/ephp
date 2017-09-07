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


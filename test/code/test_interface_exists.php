<?php

print "Traversable... ";
if (interface_exists("Traversable")) print "OK\n"; else print "NO!\n";

print "Exception...";
if (interface_exists("Exception")) print "NO!\n"; else print "OK\n";

print "NotExistInt...";
if (interface_exists("NotExistsInt")) print "NO!\n"; else print "OK\n";


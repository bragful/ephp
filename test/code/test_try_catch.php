<?php

try {
  print "hey!\n";
  throw new Exception("it's broken");
  print "no way!\n";
} catch (Exception $e) {
  print "Exception: " . $e->getMessage() . "\n";
}


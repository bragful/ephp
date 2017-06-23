<?php

try {
  print "hey!\n";
  throw new Exception("it's broken");
  print "no way!\n";
} catch (Exception $e) {
  print "Exception: " . $e->getMessage() . "\n";
} finally {
  print "and ends first one\n";
}

try {
  print "hey again!\n";
  throw new Exception("it's broken... again");
  print "no way!\n";
} catch (Exception $e) {
  throw new Exception("Another exception in chain!");
} finally {
  print "should this appears?\n";
}

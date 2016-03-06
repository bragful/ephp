<?php

if (!false):
	print "if (simple) -> OK\n";
endif;

if (true):
	print "if -> OK\n";
else:
	print "NO!\n";
endif;

for ($i=0; $i<2; $i++):
	print "loop $i\n";
endfor;
print "for -> OK\n";

foreach (array(0,1) as $i):
	print "each loop $i\n";
endforeach;
print "foreach -> OK\n";

while ($i>=0):
	print "while loop " . (--$i) . "\n";
endwhile;
print "while -> OK\n";


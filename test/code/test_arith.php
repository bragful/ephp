5 + 5 = <?=5+5?>...
(5+1)*3 = <?=(5+1)*3?>...
<?php sleep(2); ?>...
20*(10+1) = <?=20*(10+1)?>...
5/(2+10) = <?=5/(2+10)?>...

<?php

$a = time();
sleep('a');
if ((time() - $a) <= 1) print "sleep tested: OK!\n";

$a = time();
usleep('a');
if ((time() - $a) <= 1) print "usleep tested: OK!\n";

usleep(100);

?>
OK!

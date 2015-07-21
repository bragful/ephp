<?=str_replace("%body%", "black", "<body text='%body%'>")?>
<?php

$vowels = array("a", "e", "i", "o", "u", "A", "E", "I", "O", "U");
print str_replace($vowels, "", "Hello World of PHP") . "\n";

$phrase  = "You should eat fruits, vegetables, and fiber every day.";
$healthy = array("fruits", "vegetables", "fiber");
$yummy   = array("pizza", "beer", "ice cream");

print str_replace($healthy, $yummy, $phrase) . "\n";

$str = str_replace("ll", "", "good golly miss molly!", $count);
echo "count = $count\n";


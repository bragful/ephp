<?php

$f = fopen("/tmp/temporal", "w");
fwrite($f, "data to write");
fclose($f);

$f = fopen("/tmp/temporal", "r");
$msg = fread($f, 1024);
fclose($f);

?>
[<?=$msg?>]

<?php

fwrite($f, "data");

$f = fopen("/tmp/temporal", "r");
var_dump(fwrite($f, "data"));
fclose($f);


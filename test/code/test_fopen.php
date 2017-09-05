<?php

$f = fopen(__FILE__, "r");
$h = fread($f, 5);
fclose($f);

?>
[<?=$h?>]

<?php

fopen("noexiste", "r");
fclose($f);


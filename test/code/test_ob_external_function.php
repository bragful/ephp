<?php

function callback($bufer)
{
  // reemplazar todas las manzanas por naranjas
  return (str_replace("manzanas", "naranjas", $bufer));
}

ob_start("callback");

?>
<html>
<body>
<p>Es como comparar manzanas con naranjas.</p>
</body>
</html>
<?php

ob_end_flush();

ob_start();

?>
<!-- manzanas! manzanas! -->
<?

ob_flush();

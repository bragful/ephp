<?php
$num = 5;
$ubicación = 'árbol';

$formato = 'El %2$s contiene %1$d monos.' . "\n" .
           'Es un bonito %2$s con %1$d monos.' . "\n";
printf($formato, $num, $ubicación);

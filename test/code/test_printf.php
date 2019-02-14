<?php
$n =  43951789;
$u = -43951789;
$c = 65;

printf("%%b = '%b'\n", $n);
printf("%%c = '%c'\n", $c);
printf("%%d = '%d'\n", $n);
printf("%%e = '%e'\n", $n);
printf("%%u = '%u'\n", $n);
printf("%%u = '%u'\n", $u);
printf("%%f = '%f'\n", $n);
printf("%%g = '%g'\n", $n);
printf("%%o = '%o'\n", $n);
printf("%%s = '%s'\n", $n);
printf("%%x = '%x'\n", $n);
printf("%%X = '%X'\n", $n);

printf("%%+d = '%+d'\n", $n);
printf("%%+d = '%+d'\n", $u);
printf("%%+010d = '%+010d'\n", $n);
printf("%%+010d = '%+010d'\n", $u);

printf("%%h (no existent) = %h\n");
printf("%%h (no existent) = %h\n", "");
printf("%%h (no existent) = % -/#---20 \n", "");

printf("%%'.-20s = %'.-20s\n", "hello");
printf("%%'.20s  = %'.20s\n", "hello");
printf("%%2s     = %2s\n", "hello");

printf("%%20.10e = %20.10e\n", 15.51); 
printf("%%20.10E = %20.10E\n", 15.51);
printf("%%20.10f = %20.10f\n", 15.51);
printf("%%20.10F = %20.10F\n", 15.51);
printf("%%20.10g = %20.10g\n", 15.51);
printf("%%20.10G = %20.10G\n", 15.51);


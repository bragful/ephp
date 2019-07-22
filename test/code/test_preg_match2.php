<?php

$header = "nplurals=2; plural=n != 1;";
preg_match('/^\s*nplurals\s*=\s*(\d+)\s*;\s+plural\s*=\s*(.+)$/', $header, $matches);

var_dump($matches);


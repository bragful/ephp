<?php

include_once(__DIR__ . "/my_code.php");

use const MyCode\WHO;
use function MyCode\singleton;

use MyCode as MC;

singleton();
$hello = new MC\Hello;

MC\singleton();


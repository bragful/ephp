<?php

include(__DIR__ . "/function_broken.php");
include(__DIR__ . "/class_broken.php");

broken();
$broken = new broken;
$broken->broken();

<?php

$tax = new stdClass;
$tax->name = 10;
$p = isset($_POST['new' . $tax->name . '_parent']) ? (int) $_POST['new' . $tax->name . '_parent'] : 0;
var_dump($p);


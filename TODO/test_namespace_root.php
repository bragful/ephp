<?php
namespace Foo;

function strlen() {}
const INI_ALL = 3;
class Exception {}

var_dump(\strlen('hi'));
var_dump(\INI_ALL);
var_dump(new \Exception('error'));

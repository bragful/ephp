<?php

class foo
{}

class_alias('foo', 'bar');

$a = new foo;
$b = new bar;

var_dump([
	// objects are the same
	"a == b" => $a == $b,
	"a === b" => $a === $b,
	"a instance of b" => $a instanceof $b,

	// classes are the same
	"a instance of foo" => $a instanceof foo,
	"a instance of bar" => $a instanceof bar,
	"b instance of foo" => $b instanceof foo,
	"b instance of bar" => $b instanceof bar,
]);

class_alias('f', 'b');
class_alias('foo', 'bar');

var_dump(class_alias(12,12));

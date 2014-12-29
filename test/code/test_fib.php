<?

function fib($a) {
	if ($a == 1) {
		return 1;
	}
	return $a * fib($a - 1);
}

print fib(90) . "\n";


<?php

class A {
  static public function another_test($str) {
    echo "\nHello: $str\n";
    a_test($str);
  }

  public function and_another_test($str) {
    echo "\nHello: $str\n";
    A::another_test($str);
  }
}

function a_test($str) {
    echo "\nHello: $str\n";
    var_dump(debug_backtrace());
}

$a = new A;
$a->and_another_test("friend");

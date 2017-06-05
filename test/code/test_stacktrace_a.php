<?php

class A {
  public function other_test($str) {
    echo "\nHello: $str\n";
    a_test($str);
  }
}

function a_test($str) {
    echo "\nHello: $str\n";
    var_dump(debug_backtrace());
}

a_test('friend');
A::other_test('friend class');

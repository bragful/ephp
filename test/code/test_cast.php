<?php

class B { function __toString() { return "class<A>"; } }

$a = new stdClass;
$b = new B;
$c = 10;
$d = 10.5;
$e = "20";
$f = new B;
$f->greet = "hi!";

var_dump([
    (int)26.7,
    (int)"40",
    (int)45,
    (int)true,
    (int)false,
    (int)array(),
    (int)array(100),
    (int)$a,
    (int)INF,
    (int)NAN,            // 9
    (int)NULL,
    (float)"56.76",
    (float)56.76,
    (float)55,
    (float)true,
    (float)false,
    (float)array(),
    (float)array(100),
    (float)$a,
    (float)INF,          // 19
    (float)NAN,
    (float)NULL,
    (string)100,
    (string)25.52,
    (string)"world",
    (string)true,
    (string)false,
    (string)array(),
    (string)array(100),
    (string)$b,          // 29
    (string)INF,
    (string)NAN,
    (string)NULL,
    (bool)1,
    (bool)"",
    (bool)25.52,
    (bool)$c,
    (bool)$d,
    (bool)$e,
    (bool)true,          // 39
    (bool)false,
    (bool)array(),
    (bool)array(100),
    (bool)$b,
    (bool)INF,
    (bool)NAN,
    (bool)NULL,
    (array)"hello",
    (array)100,
    (array)45.54,        // 49
    (array)$f,
    (array)array(),
    (array)array(100),
    (array)INF,
    (array)NAN,
    (array)NULL,
    (array)true,
    (array)false,
    (unset)"hello",
    (unset)100,
    (unset)45.54,
    (unset)$f,
    (unset)array(),
    (unset)array(100),
    (unset)INF,
    (unset)NAN,
    (unset)NULL,
    (object)100,
    (object)45.45,
    (object)$f,
    (object)array(),
    (object)array(100),
    (object)INF,
    (object)NAN,
    (object)NULL,
]);

print (string)$a;

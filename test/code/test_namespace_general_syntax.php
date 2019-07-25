<?php
namespace my\name; // see "Defining Namespaces" section

class MyClass {
    const MYCLASSCONST = 2;
}
function myfunction() {}
const MYCONST = 1;

$a = new MyClass;
var_dump($a);
$c = new \my\name\MyClass; // see "Global Space" section
var_dump($c);

$a = strlen('hi'); // see "Using namespaces: fallback to global
                   // function/constant" section
echo "strlen = $a\n";
$d = namespace\MYCONST; // see "namespace operator and __NAMESPACE__
                        // constant" section
echo "$d\n";
$d = __NAMESPACE__ . '\MYCONST';
echo constant($d) . "\n"; // see "Namespaces and dynamic language features" section

echo namespace\MyClass::MYCLASSCONST . "\n";
echo MyClass::MYCLASSCONST . "\n";

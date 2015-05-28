<?php

$functions = get_defined_functions();
if (count($functions['internal']) > 10) {
    print "great! number of internal functions is greater than 10\n";
}
if (count($functions['user']) == 0) {
    print "great! there are not user functions registered yet\n";
}
var_dump(function_exists("strlen"));
var_dump(function_exists("function_not_exists_never"));

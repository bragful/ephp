Compatibility Table
===================

Features
--------

- Open and Close tags.
    - [x] Open tag standard `<?php`
    - [x] Open tag short `<?`
    - [x] Tag for values `<?=`
    - [x] Close tag optional
- Data types
    - [x] Numbers (with longnumbers ;-) )
    - [x] Decimals
    - [x] Booleans
    - [x] Strings
    - [x] References
    - [ ] Resources
    - [x] Objects
- Constants
    - [x] `define` function to define contants
    - [x] text without spaces as constant
    - [ ] case insensitive constants (see define function)
    - [x] "magical" constants (`__FILE__`, `__DIR__`, ...)
- Special variables
    - [x] $GLOBALS
- Arithmetic and Logic
    - [x] Sum, Substract, Multiply, Div and Module.
    - [x] `and` and `or` as operations.
    - [x] Operators for binary logic: `~`, `|`, `^`, `&`
- Loops
    - [x] for in C-style
    - [x] foreach
    - [x] while
    - [x] do...while
- Conditions
    - [x] if, if...else, if...elseif...else
    - [x] switch
- Operators for conditions
    - [x] Greater than, Greater or equals than...
    - [x] Less than, Less or equals than...
    - [x] Equals `==`, Strong equals `===`
- Spaguetti code
    - [ ] Labels
    - [ ] `goto` statement
- Modular code
    - [x] `function` definition
    - [x] insensitive name of functions
    - [x] call to functions
    - [x] args by reference
    - [ ] default values as params in functions
    - [x] include/require code from other files
- Error handling
    - [x] launch errors
    - [x] silent warnings/errors (@)
    - [ ] launch exceptions (throw)
    - [ ] catch exceptions (try...catch)
- Exit functions
    - [x] return
    - [x] die / exit
- OOP
    - [x] define class
    - [x] define methods
    - [x] construct
    - [x] destroy (delete)
    - [x] check public, private and/or protected for methods
    - [x] define attributes
    - [x] check public, private and/or protected for attributes
    - [x] define constants
    - [ ] inheritance
    - [ ] implementations (or interfaces)
    - [ ] namespaces
    - [ ] traits
- Functional
    - [x] closures


Functions
---------

 * Variables related functions:
   * [empty](http://www.php.net/empty)
   * [gettype](http://www.php.net/gettype)
   * [is_array](http://www.php.net/is_array)
   * [is_bool](http://www.php.net/is_bool)
   * [is_float](http://www.php.net/is_float) (and aliases: is_double)
   * [is_integer](http://www.php.net/is_integer) (and aliases: is_int and is_long)
   * [is_numeric](http://www.php.net/is_numeric)
   * [is_null](http://www.php.net/is_null)
   * [is_object](http://www.php.net/is_object)
   * [is_string](http://www.php.net/is_string)
   * [isset](http://www.php.net/isset)
   * [print_r](http://www.php.net/print_r)
   * [unset](http://www.php.net/unset)
   * [var_dump](http://www.php.net/var_dump) (only accepts 1 param)

 * Array related functions:
   * [count](http://www.php.net/count)
   * [in_array](http://www.php.net/in_array) (not complete)
   * [sizeof](http://www.php.net/sizeof)

 * String related functions:
   * [chr](http://www.php.net/chr)
   * [explode](http://www.php.net/explode)
   * [implode](http://www.php.net/implode)
   * [join](http://www.php.net/join)
   * [ord](http://www.php.net/ord)
   * [split](http://www.php.net/split)
   * [strlen](http://www.php.net/strlen)
   * [strtolower](http://www.php.net/strtolower)
   * [strtoupper](http://www.php.net/strtoupper)
   * [str_replace](http://www.php.net/str_replace)
   * [str_split](http://www.php.net/str_split)

 * Date/Time related functions:
   * [date](http://www.php.net/date) (not complete)
   * [date_default_timezone_get](http://www.php.net/date_default_timezone_get)
   * [date_default_timezone_set](http://www.php.net/date_default_timezone_set)
   * [gmdate](http://www.php.net/gmdate)
   * [time](http://www.php.net/time)

* Functions related functions:
   * [register_shutdown_function](http://www.php.net/register_shutdown_function)
   * [get_defined_functions](http://www.php.net/get_defined_functions)
   * [function_exists](http://www.php.net/function_exists)
   * [func_num_args](http://www.php.net/func_num_args)

* Misc functions:
   * [define](http://www.php.net/define) (not complete)
   * [die](http://www.php.net/die)
   * [exit](http://www.php.net/exit)
   * [sleep](http://www.php.net/sleep)
   * [usleep](http://www.php.net/usleep)

* Output control functions:
   * [flush](http://www.php.net/flush)
   * [ob_clean](http://www.php.net/ob_clean)
   * [ob_end_clean](http://www.php.net/ob_end_clean)
   * [ob_end_flush](http://www.php.net/ob_end_flush)
   * [ob_flush](http://www.php.net/ob_flush)
   * [ob_get_contents](http://www.php.net/ob_get_contents)
   * [ob_get_length](http://www.php.net/ob_get_length)
   * [ob_start](http://www.php.net/ob_start)

* File related functions:
   * [basename](http://www.php.net/basename)
   * [dirname](http://www.php.net/dirname)

* Class related functions:
   * [get_class](http://www.php.net/get_class)

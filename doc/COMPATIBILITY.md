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
    - [x] Resources
    - [x] Objects
    - [x] Casting/Conversion
- Strings
    - [x] Single quote string
    - [x] Double quote string
    - [x] Heredocs
- Constants
    - [x] `define` function to define contants
    - [x] text without spaces as constant
    - [ ] case insensitive constants (see define function)
    - [x] "magical" constants (`__FILE__`, `__DIR__`, ...)
- Special variables
    - [x] $GLOBALS
- Arithmetic and Logic
    - [x] Sum, Subtract, Multiply, Div and Module.
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
- Spaghetti code
    - [ ] Labels
    - [ ] `goto` statement
- Modular code
    - [x] `function` definition
    - [x] insensitive name of functions
    - [x] call to functions
    - [x] args by reference
    - [x] default values as params in functions
    - [x] include/require code from other files
- Error handling
    - [x] launch errors
    - [x] silent warnings/errors (@)
    - [x] launch exceptions (throw)
    - [x] catch exceptions (try...catch)
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
    - [x] inheritance
    - [x] implementations (or interfaces)
    - [ ] traits
- Namespaces
    - [x] Defining namespaces
    - [x] Declaring sub-namespaces
    - [x] Define multiple namespaces in the same file
    - [x] Namespaces and dynamic features
    - [x] Constant `__NAMESPACE__`
    - [ ] Aliasing/Importing (`use`)
    - [x] Fallback to global space
- Functional
    - [x] Closures

Main issues / TODO
------------------

You can check here the main issues (or incompatibilities) with PHP.

[Issues](https://github.com/bragful/ephp/issues)

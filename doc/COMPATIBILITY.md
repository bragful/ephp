Compatibility Table
===================

Features
--------

- Open and Close tags.
    - [x] Open tag standard `<?php`
    - [x] Open tag short `<?`
    - [x] Tag for values `<?=`
    - [x] Close tag optional
- Types
    - [x] Booleans
    - [x] Integers numbers (GMP mainly)
    - [x] Float point numbers
    - Strings
        - [x] Single quoted (only text)
        - [x] Double quoted (interpolated)
        - [x] Heredoc (interpolated)
        - [x] Nowdoc (only text)
    - Arrays
        - [x] Old format (`array()`)
        - [x] Optional indexing
        - [x] Mix string and integer indexes
        - [x] Auto-index adding elements
        - [x] New format (PHP 5.4+, `[]`)
        - [ ] Array dereferencing (PHP 5.4+)
    - Iterables
        - [x] Foreach iteration
        - [ ] Parameter type (for functions arguments)
        - [ ] Return type (for functions return data)
        - [ ] Iterator generator (`yield`)
        - [ ] Iterable type variance (broaden methods using array)
    - Objects
        - [x] Initialization
        - [x] Converting to object (casting)
    - [x] Resources
    - [x] NULL (internally handled by `undefined`)
    - Callable / Callbacks
        - [x] Callable as strings (function names)
        - [x] Callable as arrays (object or class and method names)
    - Pseudo-types
        - [x] Mixed
        - [x] Number
        - [x] Callable
        - [x] Array/Object
        - [x] void
        - [ ] _so on_ (`$...`)
    - [x] Type juggling
    - [x] Type casting
- Variables
    - [x] Predefined variables (superglobals)
    - Variable scope
        - [x] `global`: functions, classes and objects
        - [x] `static`: functions and classes
        - [ ] References `global` + `static`
    - [ ] Variable variables
    - Variables from external sources
        - [ ] `$_GET`, `$_POST`
        - [ ] Magic Quotes
        - [ ] Image submit
        - [ ] HTTP Cookies
        - [x] Variable types (`is_array`, `gettype`, ...)
- Constants
    - [x] Syntax
    - [x] Magic Constants (`__FILE__`, `__LINE__`, `__DIR__`, ...)
- Expressions
    - [x] Assignment
    - [x] Pre/Post-Increment/Decrement
    - [x] Comparison evaluations
    - [x] Mathematic evaluations
    - [x] Short operations with assignment
    - [x] Ternary operator
- Operators
    - [x] Precedence (see table below)
    - [x] Arithmetic operators
    - Assignment operators
        - [x] Basic assignment
        - [x] Assignment operators
        - [x] Assignment by reference
    - [x] Bitwise operators
    - Comparison operators
        - [x] Comparison operators
        - [x] Ternary operator
        - [x] Null coalesce operator
    - [x] Error control operator
    - [x] Execution operator (backticks)
    - [x] Incrementing/Decrementing operators
    - [x] Logical operators
    - [x] String operator (concat)
    - [ ] Array operators (union, equality, identity, ...)
    - [x] Type operator (`instanceof`)
- Control Structures
    - [x] Alternative syntax for control structures (with `end...` instead of curly-braces)
    - [x] `if`
    - [x] `else`
    - [x] `elseif` or `else if`
    - [x] `while`
    - [x] `do` ... `while`
    - [x] `for`
    - [x] `foreach`
    - [x] `break`
    - [x] `continue`
    - [x] `switch`
    - [ ] `declare`
    - [x] `return`
    - [x] `require` and `require_once`
    - [x] `include` and `include_once`
    - [ ] `goto`
- Functions
    - User defined functions
        - [x] Conditional functions
        - [x] Functions within functions
        - [x] Recursive functions
    - Function arguments
        - [x] Normal arguments
        - [x] Reference arguments
        - [x] Default arguments
        - [ ] Type declarations
        - [ ] Variable length argument lists
        - [ ] Variable provide arguments
    - Returning values
        - [x] Use of `return`
        - [ ] Return reference
        - [ ] Return type declarations
        - [ ] Nullable return type declaration
    - Variable functions
        - [x] Variable function/method
        - [ ] Variable static method
        - [x] Complex callables
    - Anonymous functions
        - [x] Closure class
        - [x] `use`
        - [ ] `use` passing by reference
        - [ ] Automatic binding of `$this`
        - [ ] Static anonymous functions
- Classes and Objects
    - [x] Class definition
    - [x] `new` and creation of objects
    - [x] Methods and use of `$this` pseudo-variable
    - [ ] Assignment of objects always using references
    - [x] Class methods
    - [x] Access to the object elements (`->`)
    - [x] Use of `::class` to obtain the class name
    - [x] Properties (attributes)
    - [x] Class constants
    - [x] Autoloading classes (SPL)
    - [x] Constructors and destructors
    - Visibility (`public`, `private` and `protected`)
        - [x] Properties
        - [x] Methods
        - [ ] Constants
    - [ ] Inheritance of classes (`extends`)
    - [ ] Scope resolution of operator `::`
    - `static` keyword
        - [ ] `static` to create new objects inside of class methods
        - [ ] Static variables for objects
        - [x] Static variables for methods
        - [ ] Late state bindings
    - [ ] Abstract Classes (`abstract`)
    - [ ] Interfaces (`implements`)
    - [ ] Traits (`trait`)
    - [ ] Anonymous classes
    - Overloading
        - [ ] `__call` for method overloading
        - [ ] `__callStatic` for class method overloading
        - [ ] `__get`, `__set`, `__unset` and `__isset` for properties overloading
        - [x] Object iteration (using `Iterator` for `foreach`)
        - [ ] Magic methods: `__sleep`, `__wakeup`, `__set_state`, `__clone`, `__debugInfo`
        - [x] Magic method: `__toString`
        - [x] Magic method: `__invoke` (callable)
        - [ ] `final` to avoid overloading of a method during inheritance
        - [ ] Cloning (`clone` and `__clone`)
        - [ ] Comparing objects (`==` and `===`)
        - [ ] Type hinting
        - [ ] Late state binding
        - [ ] Object serialization (`serialize` and `unserialize`)
- Namespaces
    - [x] Syntax
    - [x] Declaring simple namespaces
    - [x] Declare subnamespaces
    - [x] Multiple namespaces in a file
    - [x] Using namespaces
    - [x] Dynamic calling methods
    - [x] Use namespaces and `__NAMESPACE__` constant
    - [x] Aliasing (`use`, `use const` and `use function`)
    - [x] Global namespace (`\`)
    - [x] Fallback to the global namespace
    - [x] Name Resolution Rules
- [x] Handling errors
- Exceptions
    - [x] `try...catch...finally`
    - [x] Throw custom Exceptions (inherited)
- [ ] Generators (functions in `foreach` and use of `yield`)
- References
    - [x] Assigning by reference
    - [x] Passing by reference
    - [ ] Returning by reference
- Predefined variables
    - [x] Superglobals
    - [x] `$GLOBALS`
    - [x] `$_SERVER`, `$_ENV`
    - [ ] `$_GET`, `$_POST`, `$_REQUEST`, `$_FILES`, `$_COOKIE`, `$HTTP_POST_RAW_DATA`
    - [ ] `$_SESSION`
    - [ ] `$php_errormsg`
    - [x] `$argc`, `$argv`
- Predefined Exceptions
    - [x] Exception
    - [ ] ErrorException
    - [ ] Error
    - [ ] ArgumentCountError
    - [ ] ArithmeticError
    - [ ] AssertionError
    - [ ] DivisionByZeroError
    - [ ] CompileError
    - [ ] ParseError
    - [ ] TypeError
- Predefined Interfaces and Classes
    - [x] Traversable
    - [x] Iterator
    - [x] IteratorAggregate
    - [ ] Throwable
    - [ ] ArrayAccess
    - [ ] Serializable
    - [x] Closure
    - [ ] Generator
    - [ ] WeakReference
- Context options and parameters
    - [ ] Socket context options
    - [ ] HTTP context options
    - [ ] FTP context options
    - [ ] SSL context options
    - [ ] CURL context options
    - [ ] Phar context options
    - [ ] MongoDB context options
    - [ ] Context parameters
    - [ ] Zip context options
- Supported Protocols and Wrappers
    - [x] file://
    - [ ] http://
    - [ ] ftp://
    - [ ] php://
    - [ ] zlib://
    - [ ] data://
    - [ ] glob://
    - [ ] phar://
    - [ ] ssh2://
    - [ ] rar://
    - [ ] ogg://
    - [ ] expect://

| Associativity | Operators | Additional Information |
|:---|:---:|:---|
| non-associative | clone new | clone and new |
| right | ** | arithmetic |
| right | ++ -- ~ (int) (float) (string) (array) (object) (bool) @ | types and increment/decrement |
| non-associative | instanceof | types |
| right | ! | logical |
| left | * / % | arithmetic |
| left | + - . | arithmetic and string |
| left | << >> | bitwise |
| non-associative | < <= > >= | comparison |
| non-associative | == != === !== <> <=> | comparison |
| left | & | bitwise and references |
| left | ^ | bitwise |
| left | | | bitwise |
| left | && | logical |
| left | || | logical |
| right | ?? | null coalescing |
| left | ? : | ternary |
| right | = += -= *= **= /= .= %= &= \|= ^= <<= >>= | assignment |
| right | yield from | yield from |
| right | yield | yield |
| left | and | logical |
| left | xor | logical |
| left | or | logical |


Main issues / TODO
------------------

You can check here the main issues (or incompatibilities) with PHP.

[Issues](https://github.com/bragful/ephp/issues)

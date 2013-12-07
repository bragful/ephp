ePHP
====

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](doc/COMPATIBILITY.md)

Getting Started
---------------

A simple way to use, is include in your project `rebar.config` the following dependency line:

```
    {ephp, ".*", {git, "git://github.com/altenwald/ephp.git", master}}
```

And use the following code in your project:

```erlang
{ok, Ctx} = ephp:context_new(),
PHP = "<? $a = 5 * 23; ?>Result for $a = <?=$a?>",
{ok, Text} = ephp:eval(Ctx, PHP).
```

The result should be:

```
Result for $a = 115
```

Built-in Libs
-------------

PHP has a lot of built-in libraries. This interpreter hasn't built-in functions at the moment. I develop only a `ephp_func_date` module to show how to add new functions from Erlang to PHP. 

In near future I add more functions from the PHP specification.

You can add the libraries as:

```erlang
ephp_context:register_func(Ctx, <<"time">>, ephp_func_date, time).
```

ePHP
====

[![Build Status](https://api.travis-ci.org/altenwald/ephp.png)](https://travis-ci.org/altenwald/ephp)

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](doc/COMPATIBILITY.md).

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

The result stored in `Text` should be:

```
Result for $a = 115
```

Built-in Libs
-------------

PHP has a lot of built-in libraries. This interpreter has a little implementation of them. You can see the functions in the [compatibility table](doc/COMPATIBILITY.md).

You can add your own functions specifying the PHP name, and the module and function, as follow:

```erlang
ephp_context:register_func(Ctx, <<"time">>, ephp_func_date, time).
```

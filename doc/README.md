

# ePHP #

Copyright (c) 2013-2016 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/ephp/master.svg)](https://travis-ci.org/altenwald/ephp)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/ephp.svg)]((https://codecov.io/gh/altenwald/ephp)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/ephp.svg)](https://raw.githubusercontent.com/altenwald/ephp/master/COPYING)

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](doc/COMPATIBILITY.md).


### <a name="Requirements">Requirements</a> ###

ePHP requires to be run over an Erlang/OTP +R16, but not all the versions are full compatible or recommended. See the list:

- R16B01 **OK**
- R16B02 **OK**
- R16B03 **OK**
- R16B03-1 **OK** (recommended if you use OTP R16)
- 17.0 **OK**
- 17.1 **OK**
- 17.2 **untested**: is not available in Travis-CI
- 17.3 **NO**: fail in SSL notification to coveralls, maybe SSL prone to fail.
- 17.4 **OK**
- 17.5 **OK** (recommended if you use OTP 17)
- 18.0 **OK**
- 18.1 **OK**
- 18.2 **OK**
- 18.2.1 **OK** (recommended if you use OTP 18)
- 18.3 **OK** (tested locally, still not in travis-ci)


### <a name="Getting_Started">Getting Started</a> ###

A simple way to use, is include in your project `rebar.config` the following dependency line:

```erlang
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


### <a name="Built-in_Libs">Built-in Libs</a> ###

PHP has a lot of built-in libraries. This interpreter has a little implementation of them. You can see the functions in the [compatibility table](doc/COMPATIBILITY.md).

You can add your own functions specifying the PHP name, and the module and function, as follow:

```erlang
ephp_context:register_func(Ctx, <<"time">>, ephp_func_date, time).
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="ephp.md" class="module">ephp</a></td></tr>
<tr><td><a href="ephp_array.md" class="module">ephp_array</a></td></tr>
<tr><td><a href="ephp_class.md" class="module">ephp_class</a></td></tr>
<tr><td><a href="ephp_config.md" class="module">ephp_config</a></td></tr>
<tr><td><a href="ephp_const.md" class="module">ephp_const</a></td></tr>
<tr><td><a href="ephp_context.md" class="module">ephp_context</a></td></tr>
<tr><td><a href="ephp_error.md" class="module">ephp_error</a></td></tr>
<tr><td><a href="ephp_func.md" class="module">ephp_func</a></td></tr>
<tr><td><a href="ephp_include.md" class="module">ephp_include</a></td></tr>
<tr><td><a href="ephp_interpr.md" class="module">ephp_interpr</a></td></tr>
<tr><td><a href="ephp_lib_array.md" class="module">ephp_lib_array</a></td></tr>
<tr><td><a href="ephp_lib_class.md" class="module">ephp_lib_class</a></td></tr>
<tr><td><a href="ephp_lib_control.md" class="module">ephp_lib_control</a></td></tr>
<tr><td><a href="ephp_lib_date.md" class="module">ephp_lib_date</a></td></tr>
<tr><td><a href="ephp_lib_file.md" class="module">ephp_lib_file</a></td></tr>
<tr><td><a href="ephp_lib_func.md" class="module">ephp_lib_func</a></td></tr>
<tr><td><a href="ephp_lib_info.md" class="module">ephp_lib_info</a></td></tr>
<tr><td><a href="ephp_lib_math.md" class="module">ephp_lib_math</a></td></tr>
<tr><td><a href="ephp_lib_misc.md" class="module">ephp_lib_misc</a></td></tr>
<tr><td><a href="ephp_lib_ob.md" class="module">ephp_lib_ob</a></td></tr>
<tr><td><a href="ephp_lib_string.md" class="module">ephp_lib_string</a></td></tr>
<tr><td><a href="ephp_lib_vars.md" class="module">ephp_lib_vars</a></td></tr>
<tr><td><a href="ephp_output.md" class="module">ephp_output</a></td></tr>
<tr><td><a href="ephp_parser.md" class="module">ephp_parser</a></td></tr>
<tr><td><a href="ephp_shutdown.md" class="module">ephp_shutdown</a></td></tr>
<tr><td><a href="ephp_util.md" class="module">ephp_util</a></td></tr>
<tr><td><a href="ephp_vars.md" class="module">ephp_vars</a></td></tr></table>


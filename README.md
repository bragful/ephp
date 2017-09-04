

# ePHP #

Copyright (c) 2013-2017 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/bragful/ephp/master.svg)](https://travis-ci.org/bragful/ephp)
[![Codecov](https://img.shields.io/codecov/c/github/bragful/ephp.svg)](https://codecov.io/gh/bragful/ephp)
[![License: LGPL 2.1](https://img.shields.io/github/license/bragful/ephp.svg)](https://raw.githubusercontent.com/bragful/ephp/master/COPYING)

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](http://github.com/bragful/ephp/blob/master/doc/COMPATIBILITY.md).


### <a name="Requirements">Requirements</a> ###

ePHP requires to be run over an Erlang/OTP +R16, but not all the versions are full compatible or recommended. See the list:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 20.0 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |
| 18.3 | :heavy_check_mark: | Recommended if you use OTP 18 |
| 18.2.1 | :heavy_check_mark: | |
| 18.2 | :heavy_check_mark: | |
| 18.1 | :heavy_check_mark: | |
| 18.0 | :heavy_check_mark: | |
| 17.5 | :heavy_check_mark: | Recommended if you use OTP 17 |
| 17.4 | :heavy_check_mark: | |
| 17.3 | :x: | fail in SSL |
| 17.2 | :x: | no tests available in Travis-CI |
| 17.1 | :heavy_check_mark: | |
| 17.0 | :heavy_check_mark: | |
| R16B03-1 | :heavy_check_mark: | Recommended if you use OTP R16 |
| R16B03 | :heavy_check_mark: | |
| R16B02 | :heavy_check_mark: | |
| R16B01 | :x: | fails in math lib |


### <a name="Getting_Started">Getting Started</a> ###

A simple way to use, is include in your project `rebar.config` the following dependency line:

```erlang
    {ephp, ".*", {git, "git://github.com/bragful/ephp.git", master}}
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

PHP has a lot of built-in libraries. This interpreter has a little implementation of them. You can see the functions in the [compatibility table](http://github.com/bragful/ephp/blob/master/doc/COMPATIBILITY.md).

You can add your own functions specifying the PHP name, and the module and function, as follow:

```erlang
ephp_context:register_func(Ctx, <<"time">>, ephp_lib_date, time).
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp.md" class="module">ephp</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_array.md" class="module">ephp_array</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_class.md" class="module">ephp_class</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_class_exception.md" class="module">ephp_class_exception</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_config.md" class="module">ephp_config</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_const.md" class="module">ephp_const</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_context.md" class="module">ephp_context</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_cover.md" class="module">ephp_cover</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_data.md" class="module">ephp_data</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_datetime.md" class="module">ephp_datetime</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_error.md" class="module">ephp_error</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_func.md" class="module">ephp_func</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_include.md" class="module">ephp_include</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_interpr.md" class="module">ephp_interpr</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_array.md" class="module">ephp_lib_array</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_class.md" class="module">ephp_lib_class</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_control.md" class="module">ephp_lib_control</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_date.md" class="module">ephp_lib_date</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_error.md" class="module">ephp_lib_error</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_file.md" class="module">ephp_lib_file</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_func.md" class="module">ephp_lib_func</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_info.md" class="module">ephp_lib_info</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_math.md" class="module">ephp_lib_math</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_misc.md" class="module">ephp_lib_misc</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_ob.md" class="module">ephp_lib_ob</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_pcre.md" class="module">ephp_lib_pcre</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_spl.md" class="module">ephp_lib_spl</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_string.md" class="module">ephp_lib_string</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_vars.md" class="module">ephp_lib_vars</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_mem.md" class="module">ephp_mem</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_object.md" class="module">ephp_object</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_output.md" class="module">ephp_output</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser.md" class="module">ephp_parser</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_class.md" class="module">ephp_parser_class</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_expr.md" class="module">ephp_parser_expr</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_func.md" class="module">ephp_parser_func</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_string.md" class="module">ephp_parser_string</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_shutdown.md" class="module">ephp_shutdown</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_stack.md" class="module">ephp_stack</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_string.md" class="module">ephp_string</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_timezone.md" class="module">ephp_timezone</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_vars.md" class="module">ephp_vars</a></td></tr></table>


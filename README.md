

# ePHP #

Copyright (c) 2013-2019 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/bragful/ephp/master.svg)](https://travis-ci.org/bragful/ephp)
[![Codecov](https://img.shields.io/codecov/c/github/bragful/ephp.svg)](https://codecov.io/gh/bragful/ephp)
[![License: LGPL 2.1](https://img.shields.io/github/license/bragful/ephp.svg)](https://raw.githubusercontent.com/bragful/ephp/master/COPYING)
[![Gitter](https://img.shields.io/gitter/room/bragful/ephp.svg)](https://gitter.im/bragful/ephp)
[![Hex](https://img.shields.io/hexpm/v/ephp.svg)](https://hex.pm/packages/ephp)

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](http://github.com/bragful/ephp/blob/master/doc/COMPATIBILITY.md).


### <a name="Donation">Donation</a> ###

If you want to support the project to advance faster with the development you can make a donation. Thanks!

[![paypal](https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CBYJ5V2ZWWZ8G)


### <a name="Why_PHP_over_Erlang?">Why PHP over Erlang?</a> ###

When I was leading a team of developers in Spain, in Telco companies the most in use language was PHP. Even for handling concurrency, they were using some techniques with POSIX mutexes, shared memory and nohup background processes. That was a nightmare.

We discover Erlang these days (back in 2009) and started to use it to solve all of those issues and create a more reliable software. But the rest of the companies and even our clients continued using and requesting to make changes using PHP. This way I discover that we could gave them the flexibility of PHP as logic language to develop their code and the reliability of Erlang virtual machine (BEAM).

We tried, however, to use previously other solutions like SpiderMonkey or V8 to use JavaScript like other projects do (Riak and CouchDB), but we realize those solutions create the dependency of an external library which isn't working as a breeze most of the times. And the requirement was about to use PHP and not other different languages.

The motivation was: have a 100% Erlang code PHP interpreter. No C extensions and trying to minimize as much as possible the dependencies to have as little as possible the interpreter. That's because JSON, MySQL and other extensions are in different repositories letting you to include them in your code or not, depending on your needs.

In 2016, after fight again and again against the same issue (heredoc), I decided to rewrite the parser in 100% Erlang code. This let me to get rid of neotoma and speed up a bit the parsing of the code. Indeed, this let me to solve the heredoc issue and continue implementing the rest of the PHP syntax easily.

In 2019, ePHP was published finally as a hex package. In addition, I developed a new library for Elixir which shows how to use ephp for Elixir projects and add the ability to use PHP templates for [Phoenix Framework](https://phoenixframework.org/) projects: [ephp_template](https://github.com/bragful/ephp_template).

You can see even an example of ephp in use for the running on-the-fly of PHP code to play the [Leprechaun game](https://leprechaun.altenwald.com) ([source code here](https://github.com/altenwald/leprechaun)).

And a post to know how to write a stand-alone server with Erlang, without write a single line of Erlang code and using PHP for the logic of a XMPP component [here](https://medium.com/@bosqueviejo/developing-a-bot-using-tdd-on-erlang-without-write-a-single-line-of-code-in-erlang-5278e28b0356).


### <a name="Requirements">Requirements</a> ###

ePHP requires to be run over an Erlang/OTP 19+, but not all the versions are full compatible or recommended. See the list:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 22.2 | :heavy_check_mark: | Recommended if you use OTP 22 |
| 22.1 | :heavy_check_mark: | |
| 22.0 | :heavy_check_mark: | |
| 21.3 | :heavy_check_mark: | Recommended if you use OTP 21 |
| 21.2 | :heavy_check_mark: | |
| 21.1 | :heavy_check_mark: | |
| 21.0 | :heavy_check_mark: | |
| 20.3 | :x: | fails in math lib |
| 20.2 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 20.1 | :heavy_check_mark: | |
| 20.0 | :heavy_check_mark: | |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |


### <a name="Getting_Started">Getting Started</a> ###

A simple way to use, is include in your project `rebar.config` the following dependency line:

```erlang
    {ephp, "0.2.7"}
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
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_class_closure.md" class="module">ephp_class_closure</a></td></tr>
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
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib.md" class="module">ephp_lib</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_array.md" class="module">ephp_lib_array</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_class.md" class="module">ephp_lib_class</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_control.md" class="module">ephp_lib_control</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_date.md" class="module">ephp_lib_date</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_error.md" class="module">ephp_lib_error</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_lib_exec.md" class="module">ephp_lib_exec</a></td></tr>
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
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_ns.md" class="module">ephp_ns</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_object.md" class="module">ephp_object</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_output.md" class="module">ephp_output</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser.md" class="module">ephp_parser</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_class.md" class="module">ephp_parser_class</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_expr.md" class="module">ephp_parser_expr</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_func.md" class="module">ephp_parser_func</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_parser_string.md" class="module">ephp_parser_string</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_shutdown.md" class="module">ephp_shutdown</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_stack.md" class="module">ephp_stack</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_stream.md" class="module">ephp_stream</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_stream_file.md" class="module">ephp_stream_file</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_string.md" class="module">ephp_string</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_timezone.md" class="module">ephp_timezone</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_tracer.md" class="module">ephp_tracer</a></td></tr>
<tr><td><a href="http://github.com/bragful/ephp/blob/master/doc/ephp_vars.md" class="module">ephp_vars</a></td></tr></table>


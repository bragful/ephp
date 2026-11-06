# ePHP

[![Build Status](https://github.com/bragful/ephp/actions/workflows/erlang.yml/badge.svg)](https://github.com/bragful/ephp/actions/workflows/erlang.yml)
[![Codecov](https://img.shields.io/codecov/c/github/bragful/ephp.svg)](https://codecov.io/gh/bragful/ephp)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://raw.githubusercontent.com/bragful/ephp/master/LICENSE)
[![Hex](https://img.shields.io/hexpm/v/ephp.svg)](https://hex.pm/packages/ephp)

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](http://github.com/bragful/ephp/blob/master/doc/COMPATIBILITY.md).

### Who's using it?

At the moment, this project has been tested using Kazoo and connected to FreeSwitch for writing small VoIP controlling apps.

In addition, it was tested using XMPP in the Snatch Project (deprecated).

You can find [here](https://github.com/bragful/ephp_template) an implementation for Phoenix Framework to use PHP instead of EEx for some pages.

If you are using **ePHP** and you are proud about you created, please let me know for extending the list.

### Donation

If you want to support the project to advance faster with the development you can make a donation. Thanks!

[![paypal](https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=PU85KWS9UZMKA)


### Why PHP over Erlang?

When I was leading a team of developers in Spain, in Telco companies the most in use language was PHP. Even for handling concurrency, they were using some techniques with POSIX mutexes, shared memory and nohup background processes. That was a nightmare.

We discover Erlang these days (back in 2009) and started to use it to solve all of those issues and create a more reliable software. But the rest of the companies and even our clients continued using and requesting to make changes using PHP. This way I discover that we could gave them the flexibility of PHP as logic language to develop their code and the reliability of Erlang virtual machine (BEAM).

We tried, however, to use previously other solutions like SpiderMonkey or V8 to use JavaScript like other projects do (Riak and CouchDB), but we realize those solutions create the dependency of an external library which isn't working as a breeze most of the times. And the requirement was about to use PHP and not other different languages.

The motivation was: have a 100% Erlang code PHP interpreter. No C extensions and trying to minimize as much as possible the dependencies to have as little as possible the interpreter. That's because JSON, MySQL and other extensions are in different repositories letting you to include them in your code or not, depending on your needs.

In 2016, after fight again and again against the same issue (heredoc), I decided to rewrite the parser in 100% Erlang code. This let me to get rid of neotoma and speed up a bit the parsing of the code. Indeed, this let me to solve the heredoc issue and continue implementing the rest of the PHP syntax easily.

In 2019, ePHP was published finally as a hex package. In addition, I developed a new library for Elixir which shows how to use ephp for Elixir projects and add the ability to use PHP templates for [Phoenix Framework](https://phoenixframework.org/) projects: [ephp_template](https://github.com/bragful/ephp_template).

You can see even an example of ephp in use for the running on-the-fly of PHP code to play the [Leprechaun game](https://leprechaun.altenwald.com) ([source code here](https://github.com/altenwald/leprechaun)).

And a post to know how to write a stand-alone server with Erlang, without write a single line of Erlang code and using PHP for the logic of a XMPP component [here](https://medium.com/@bosqueviejo/developing-a-bot-using-tdd-on-erlang-without-write-a-single-line-of-code-in-erlang-5278e28b0356).


### Requirements

ePHP requires to be run over an Erlang/OTP 26+.

### Getting Started

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

### License

ePHP is licensed under the [Apache License, Version 2.0](LICENSE).

Copyright (c) 2013-2026 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

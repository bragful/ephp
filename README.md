# ePHP

[![Build Status](https://github.com/bragful/ephp/actions/workflows/erlang.yml/badge.svg)](https://github.com/bragful/ephp/actions/workflows/erlang.yml)
[![Codecov](https://img.shields.io/codecov/c/github/bragful/ephp.svg)](https://codecov.io/gh/bragful/ephp)
[![License: LGPL 2.1](https://img.shields.io/github/license/bragful/ephp.svg)](https://raw.githubusercontent.com/bragful/ephp/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/ephp.svg)](https://hex.pm/packages/ephp)

PHP Interpreter pure 100% Erlang. This interpreter was made for enhance and give flexibility to projects that requires an interface for plugins or addons without new compilations.

In the same way, you can use for server PHP pages in an easy way.

The port is not 100% complete, please refer to [compatibility table](http://github.com/bragful/ephp/blob/master/doc/COMPATIBILITY.md).

> ### IMPORTANT {: .warning}
>
> The generation of the documentation is made using `edown` which isn't compatible with OTP 23 and OTP 24 at the moment. If you need to generate the documentation, install an OTP 22 and run `make doc` removing the `_build` directory if you compiled previously with a more recent OTP version.


### Who's using it?

At the moment, this project has been tested using Kazoo and connected to FreeSwitch for writing small VoIP controlling apps.

In addition, it was tested using XMPP in the Snatch Project (deprecated).

You can find [here](https://github.com/bragful/ephp_template) an implementation for Phoenix Framework to use PHP instead of EEx for some pages.

And as embedded project, it's planned to be included in [Conta](https://github.com/altenwald/conta).

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

ePHP requires to be run over an Erlang/OTP 19+, but not all the versions are full compatible or recommended. See the list:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 27.0 | ✓ | Recommended if you use OTP 27 |
| 26.2 | 𐄂 | fails compiling parser |
| 26.1 | 𐄂 | fails compiling parser |
| 26.0 | ✓ | Recommended if you use OTP 26 |
| 25.3 | ✓ | Recommended if you use OTP 25 |
| 25.2 | ✓ | |
| 25.1 | ✓ | |
| 25.0 | ✓ | |
| 24.3 | ✓ | Recommended if you use OTP 24 |
| 24.2 | ✓ | |
| 24.1 | ✓ | |
| 24.0 | ✓ | |
| 23.3 | ✓ | Recommended if you use OTP 23 |
| 23.2 | ✓ | |
| 23.1 | ✓ | |
| 23.0 | ✓ | |
| 22.3 | ✓ | Recommended if you use OTP 22 |
| 22.2 | ✓ | |
| 22.1 | ✓ | |
| 22.0 | ✓ | |
| 21.3 | ✓ | Recommended if you use OTP 21 |
| 21.2 | ✓ | |
| 21.1 | ✓ | |
| 21.0 | ✓ | |
| 20.3 | 𐄂 | fails in math lib |
| 20.2 | ✓ | Recommended if you use OTP 20 |
| 20.1 | ✓ | |
| 20.0 | ✓ | |
| 19.3 | ✓ | Recommended if you use OTP 19 |
| 19.2 | ✓ | |
| 19.1 | ✓ | |
| 19.0 | ✓ | |


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

### Controversial License

Yes, looks like not so many people like GPL lincenses and they didn't realised that LGPL 1.2 is very light license in comparison. I mean, I'm the author and it's not my aim to sue to the people for using this software. LGPL is only requesting feedback in case you change and improve the library, that's all. In addition, if you are using it for a project and you have a credits section, it could be great to be included.

It's NOT required your project will be LGPL or even open source. I think it's the most important part.

Copyright (c) 2013-2024 Altenwald

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

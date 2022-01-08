---
title: "Getting Erlang docs in Elixir"
date: 2022-02-30 13:15:02
categories: ["elixir"]
---

You may have heard that Elixir developers can now get access to Erlang documentation in `iex`. However, if you are like me you may find that rather than getting message indicating you cannot get access to Elixir-style docs

```
iex> h :rand
:rand is an Erlang module and, as such, it does not have Elixir-style docs
```

you are now told that it wasn't _compiled_ with docs

```ex
iex> h :rand
:rand was not compiled with docs
```

In the post, I will cover how you can compile your Erlang to documentation that iss accessible via `iex`.

## Show me

Before I get into how to install Erlang with the documentation, I want what you get for your troubles.

**Module-level documentation**

```ex
iex> h :rand

                                     :rand

This module provides a pseudo random number generator. The module contains a
number of algorithms. The uniform distribution algorithms are based on the
Xoroshiro and Xorshift algorithms  (http://xorshift.di.unimi.it)by Sebastiano
Vigna. The normal distribution algorithm uses the Ziggurat Method by Marsaglia
and Tsang  (http://www.jstatsoft.org/v05/i08)on top of the uniform distribution
algorithm.

(...)
```

**Function-level documentation**

```ex

iex> h :rand.uniform/1

                                   uniform/1

  @spec uniform(n :: pos_integer()) :: x :: pos_integer()

since: OTP 18.0

Returns, for a specified integer N >= 1, a random integer uniformly distributed
in the value range 1 =< X =< N and updates the state in the process dictionary.
```

## Installation

### Requirements

See the [history](#history) section for more details, but, suffice to say, you will need to be using OTP 23+ and Elixir 1.7+.

I use [asdf](https://asdf-vm.com/) to manage my installations of [Erlang](https://github.com/asdf-vm/asdf-erlang) and [Elixir](https://github.com/asdf-vm/asdf-elixir) so I will be using that as my example. Check out their [getting started](https://asdf-vm.com/guide/getting-started.html#_1-install-dependencies) page to get set up. Also, `asdf-erlang` uses [`kerl`](https://github.com/kerl/kerl) to manage installing Elixir and the flags we are setting are for `kerl`, so you should be able to apply the same techniques if you use `kerl`.

Depending on your OS, there may be additional dependencies you need to install. See [this section](https://github.com/asdf-vm/asdf-erlang#before-asdf-install) to see if there is anything you need. Most likely, you will need to use your package manager of choice to isntall `libxslt` and `fop`. However, I don't think `fop` is _strictly_ necessary as I receive the following:

```bash
DOCUMENTATION INFORMATION (See: ~/.asdf/plugins/erlang/kerl-home/builds/asdf_24.1.7/otp_build_24.1.7.log)
 * documentation  :
 *                  fop is missing.
 *                  Using fakefop to generate placeholder PDF files.
```

### Compiling with Docs

The [docs](https://github.com/asdf-vm/asdf-erlang#getting-erlang-documentation) for `asdf-erlang` cover the different `kerl` flags, set as environment vairables, for working with documentation:

- `KERL_BUILD_DOCS` - set to `yes` to install documentation
- `KERL_INSTALL_HTMLDOCS` - whether or not to additionally install HTML docs
- `KERL_INSTALL_MANDOCS` - whether or not to additionally install Man page-style docs

The `asdf-erlang` maintainers suggest not including these additional formats:

> It may be a good idea to disable those formats to save space, since docs can easily take around 200MB in addition to 100MB of base installation, yet to still have docs inside shell.

Since we are only concerned with getting access to the docs within IEx, we will heed their advice.

### One-time install

Since the `kerl` configuration happens via enironment variables, we can set them in-line when installing Erlang:

```bash
KERL_BUILD_DOCS=yes KERL_INSTALL_HTMLDOCS=no KERL_INSTALL_MANDOCS=no \
asdf install erlang <VERSION>
```

at the time of writing, `24.2` is the newest version of Erlang available via `asdf`

```bash
› asdf list-all erlang | tail -1
24.2
```

Let's use your command to install that version:

```bash
KERL_BUILD_DOCS=yes KERL_INSTALL_HTMLDOCS=no KERL_INSTALL_MANDOCS=no \
asdf install erlang 24.2
```

When that is done building, we can install the most recently available version of Elixir (`1.13.1`) that is compatiple with OTP 24. We can do this with a plain `asdf install`, no environment variables or special flags necesasry!

```bash
asdf install elixir 1.13.1-otp-24
```

#### Future proof

If you plan to install new versions of Erlang/OTP in the future and expect to still want documentation, you may not want to have to remember these flags (though you can visit this post again!).

To avoid this in the future, you may want to consider setting your environment variables in a more permanent manner via your shell of choice's configuration. For example, since I am using `zsh`, I could put the following in my `~/.zshrc`

```bash
# ~/.zshrc

# Configuration for compiling Erlang with
# documentation that I can use in IEx
#
# See  https://github.com/asdf-vm/asdf-erlang#getting-erlang-documentation
# for more details
KERL_BUILD_DOCS=yes
KERL_INSTALL_HTMLDOCS=no
KERL_INSTALL_MANDOCS=no
```

Now, future Erlang installing will automatically have the proper flags set and install the documentation for me!

## History

Strictly unnecessary, but here are some links to events related to the development around getting access to Erlang's documentation within Elixir.

Doing some GitHub spelunking, it looks like [this Issue](https://github.com/elixir-lang/elixir/issues/3589) from 2015 started the discussion of getting Erlang documentation to be displayable via the `h` helper. Over the course of the thread, a [possible PR](https://github.com/elixir-lang/elixir/pull/3640) was opened, a [library created](https://github.com/philosophers-stone/ehelper). In 2018, José announced a posted proposal, [EEP 48](https://www.erlang.org/eeps/eep-0048) to the [Erlang Enhancement Procees](https://www.erlang.org/eep). The goal of this proposal was to standardize how all BEAM languages work with documentation:

> This EEP proposes an official API documentation storage to be used by by BEAM languages. By standardizing how API documentation is stored, it will be possible to write tools that work across languages.

The EEP was posted in Januaray 2018 and in July 2018, [Elixir 1.7 was released](https://github.com/elixir-lang/elixir/releases/tag/v1.7.0) with an impementation of EEP 48:

> Elixir v1.7 implements EEP 48. EEP 48 aims to bring documentation interoperability across all languages running on the Erlang VM. The documentation format proposed by EEP 48 also supports metadata, which is now fully exposed to Elixir developers.

At this point, Elixir developers would gain access to new metadata like the `since` keyword, but no Erlang documentation.

```ex{5}
iex> h Kernel.tap

                            defmacro tap(value, fun)

since: 1.12.0

Pipes value to the given fun and returns the value itself.

Useful for running synchronous side effects in a pipeline.
```

Erlang support would come as a part of [OTP 23](https://github.com/erlang/otp/releases/tag/OTP-23.0) in May 2020:

> New functions in the shell for displaying documentation for Erlang modules, functions and types. The
> functions are:
> (...)
> The embedded documentation is created as docchunks (EEP 48) when building the Erlang/OTP documentation.

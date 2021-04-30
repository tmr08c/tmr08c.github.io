---
title: "Using Mix.install"
date: "2021-06-26T06:31:13.265Z"
categories: ["elixir"]
---

Despite being a compiled language, Elixir has provided support for executing code in file as though it were a scripting language since it's inception. By using the `.exs` extension, Elixir will compile the file in-memory and run the compiled code.

As a simple example, we can create a file, `my_script.exs` to print something "Hello, World"-like.

```elixir
# my_script.exs
IO.puts("Hello, from Elixir")
IO.puts("...a scripting language?")
```

We can run `.exs` by passing the pathname to the `elixir` command.

```bash
› elixir my_script.exs

Hello, from Elixir
...a scripting language?
```

[This article](https://thinkingelixir.com/2019-04-running-an-elixir-file-as-a-script/) covers more of how this works and some of the downsides. One downside not mentioned in the article is the inability to use packages. Since `exs` scripts are not intended to be complicated programs it makes sense this was not mentioned.

# Notes

Installing and using verbose

```
Mix.install([:jason], verbose: true)
```

First run

```
using /Users/troyrosenberg/Library/Caches/mix/installs/elixir-1.12.0-rc.1-erts-12.0/11989020f314102159a0c9ca882052fc
Resolving Hex dependencies...
Dependency resolution completed:
New:
  jason 1.2.2
* Getting jason (Hex package)
==> jason
Compiling 8 files (.ex)
Generated jason app
{"hello":"world"}
```

Link to =escript= at end as compairson

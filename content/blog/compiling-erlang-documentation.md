---
title: "Compiling Erlang Documentation"
date: "2022-08-31T18:08:13.265Z"
categories: ["erlang", "elixir", "asdf", "documentation"]
---

If you build Erlang with `kerl` (which `asdf` does), you can configure it to build documentation for you by setting the environment variable `KERL_BUILD_DOCS` to `yes`.

This will HTML docs (configurable via `KERL_INSTALL_HTMLDOCS`), Man pages (configurable via `KERL_INSTALL_MANPAGES`) and, thanks to [EEP 48](https://www.erlang.org/eeps/eep-0048) (requiring OTP 23+), embedded documentation that you can access via the REPL (`erl`, and even `iex` for Elixir).

See the [asdf-erlang](https://github.com/asdf-vm/asdf-erlang#getting-erlang-documentation) documentation for more.

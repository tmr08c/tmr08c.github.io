---
title: "Compiling Erlang Documentation"
date: "2022-09-30T08:51:13.265Z"
categories: ["erlang", "elixir", "asdf", "documentation"]
---

If you install Erlang with `kerl` (which `asdf` does), you can configure it to build its documentation by setting the environment variable `KERL_BUILD_DOCS` to `yes`.

By default, `kerl` will build HTML docs (configurable via `KERL_INSTALL_HTMLDOCS`), Man pages (configurable via `KERL_INSTALL_MANPAGES`) and, thanks to [EEP 48](https://www.erlang.org/eeps/eep-0048) (requiring OTP 23+), embedded documentation that you can access via the REPL (`erl`, and even `iex` for Elixir) and LSP. For Elixirists, we can now use the [`h/1`](https://hexdocs.pm/iex/IEx.Helpers.html#h/1) IEx helper on Erlang modules and functions.

```
iex> h :ets.insert
h :ets.insert

                                    insert/2

  @spec insert(table, objectOrObjects) :: true
        when table: table(), objectOrObjects: tuple() | [tuple()]

Inserts the object or all of the objects in list ObjectOrObjects into table
Table.

  • If the table type is set and the key of the inserted objects matches the key of
    any object in the table, the old object is replaced.

  • If the table type is ordered_set and the key of the inserted object compares
    equal to the key of any object in the table, the old object is replaced.

  • If the list contains more than one object with matching keys and the table type
    is set, one is inserted, which one is not defined. The same holds for table
    type ordered_set if the keys compare equal.

The entire operation is guaranteed to be atomic and isolated
(stdlib:ets#concurrency), even when a list of objects is inserted.
```

See the [asdf-erlang](https://github.com/asdf-vm/asdf-erlang#getting-erlang-documentation) documentation for more.

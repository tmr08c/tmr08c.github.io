---
date: '2021-05-28T07:01:13'
tags:
- elixir
title: Using Mix.install
---

Despite being a compiled language, Elixir has provided support for running code in a single file as though it were a scripting language since its release. When processing a file with the `.exs` extension Elixir will compile the file in-memory and run the compiled code. In this post, we will cover an experimental feature that is expanding upon the language's scripting abilities.

## A Basic Elixir Script

To introduce scripting with Elixir, we can create a file, `my_script.exs`, to print something "Hello, World"-like.

```elixir
# my_script.exs
IO.puts("Hello, from Elixir")
IO.puts("...a scripting language?")
```

We can run our `.exs` file by passing its pathname to the `elixir` command.

```bash
› elixir my_script.exs

Hello, from Elixir
...a scripting language?
```

[This article](https://thinkingelixir.com/2019-04-running-an-elixir-file-as-a-script/) covers more on how this works.

## Bringing in Dependencies

To further extend Elxir's ability to be used in this scripting context, a new [experimental feature](https://github.com/elixir-lang/elixir/pull/10674) has been added in Elixir's [1.12 release](https://github.com/elixir-lang/elixir/releases/tag/v1.12.0), [`Mix.install`](https://hexdocs.pm/mix/1.12.0/Mix.html#install/2). With `Mix.install`, you can list third-party packages to use in your script like you would in a `mix.exs` file. If you are familiar with the Ruby ecosystem, this is similar to the [inline](https://bundler.io/guides/bundler_in_a_single_file_ruby_script.html) functionality provided by Bundler.

## A Basic `Mix.install`

To start with something simple, let's steal an example from the documentation: JSON-encoding a map with the [Jason](https://github.com/michalmuskala/jason) package.

```elixir
# mix_install_test.exs
Mix.install([:jason])
IO.puts(Jason.encode!(%{hello: :world}))
```

When running, we see the following output:

```bash
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

We first resolve, fetch, and compile our dependencies; this should look familiar to you if you've used `mix deps.get` before. Once `Mix.install` is complete and we have our dependencies, we run our `IO.puts` and output the encoded JSON.

## Caching

Our dependencies are cached as a part of our first run; this means subsequent runs will not need to include any dependency management!

```bash
› elixir mix_install_test.exs
# no resolving dependencies!
{"hello":"world"}
```

To find out where the dependencies are cached on your system, you can pass the `verbose` option to `Mix.install`.

```elixir
Mix.install(
  [:jason],
  verbose: true
)
```

Now, when we run our script, it will output the path to the dependency cache.

```bash
› elixir mix_install_test.exs
using /Users/me/Library/Caches/mix/installs/elixir-1.12.0-erts-12.0/11989020f314102159a0c9ca882052fc
```

Caching [is based](https://github.com/elixir-lang/elixir/blob/3c7e3bd67d3c78c746a7db359da505e688a6f504/lib/mix/lib/mix.ex#L555-L557) on a combination of Elixir and OTP versions, as well as the dependencies you have listed. Changing any of these (or setting the [`force` flag](https://hexdocs.pm/mix/1.12.0/Mix.html#install/2-options)) will result in a cache miss and require re-fetching and compiling packages.

## Mix Options

`Mix.install` works by [dynamically building a Mix project](https://github.com/elixir-lang/elixir/blob/3c7e3bd67d3c78c746a7db359da505e688a6f504/lib/mix/lib/mix.ex#L567-L582) for you when the script runs. As a result, the dependency list passed to `Mix.install` is the same as your `deps` list in a Mix project's `mix.exs`. By working with an in-memory Mix project, you can take full advantage of Mix dependency management, including specifying package versions and all other [options](https://hexdocs.pm/mix/Mix.Tasks.Deps.html#module-options) provided by Mix.

## Another Example

One possible use case I imagined for using `Mix.install` was to explore new APIs. As an example, I wanted to fetch the current price of Bitcoin from the [Coinbase API](https://developers.coinbase.com/).

When creating the script, I decided to use [Mojito](https://github.com/appcues/mojito) for HTTP requests. Unfortunately, I ran into an issue using OTP 24 with [Mint](https://github.com/elixir-mint/mint), the underlying packages Mojito is built on. This issue [was resolved](https://github.com/elixir-mint/mint/pull/293) on the `main` branch, but had not yet been released. By leveraging the [git options](https://hexdocs.pm/mix/Mix.Tasks.Deps.html#module-git-options-git) provided by Mix, I was able to point at the `main` branch to use the latest code. Since I was _actually_ using Mojito, I was also able to leverage the `override` option to tell Mix to use my overridden version of the dependency. Because `Mix.Install` provides the full power of Mix dependency management, I was able to easily work around the temporary issues and create a script that solved my problem.

```elixir
Mix.install(
  [
    :jason,
    :mojito,
    {
      :mint,
      git: "https://github.com/elixir-mint/mint",
      branch: "main",
      override: true
    }
  ]
)

{:ok, %{body: body}} =
  Mojito.request(
    method: :get,
    url: "https://api.coindesk.com/v1/bpi/currentprice.json"
  )

bit_coin_rate =
  body
  |> Jason.decode!()
  |> get_in(["bpi", "USD", "rate"])

IO.puts("The current rate for Bitcoin is #{bit_coin_rate}")
```

We can now run this program as though it were a Ruby or Python script to track our Bitcoin investment 💎 🙌 🚀!

```bash
› elixir bitcoin_price.exs
The current rate for Bitcoin is 55,277.6167

› elixir bitcoin_price.exs
The current rate for Bitcoin is 55,168.1100
```

## Future

Because we still need Elixir installed on our system to run the script, this doesn't provide us with the portability of something like [Go's ability to build executable binaries](https://www.digitalocean.com/community/tutorials/how-to-build-and-install-go-programs). For that, you may want to explore [Bakeware](https://github.com/bake-bake-bake/bakeware). For something between a single file script and `Bakeware`, you may also want to investigate [escript](https://hexdocs.pm/mix/master/Mix.Tasks.Escript.Build.html). With `escript`, you can build your `mix` project into an executable. It does, however, require Erlang to be installed on the system running the program.

While you may need to reach for more robust options, `Mix.install` provides another tool in the Elixir toolbelt. With the addition of a single function, Elixir has increased its abilities to write small scripts and be used in more ways.

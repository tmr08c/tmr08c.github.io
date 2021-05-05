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

However, to further extend the ability for Elixir to be used in this scripting context, a new, experimental feature has [been added](https://github.com/elixir-lang/elixir/pull/10674) in 1.12, [`Mix.Install`](https://hexdocs.pm/mix/1.12.0-rc.0/Mix.html#install/2). With `Mix.Install`, you can list dependencies to install just like you would in a `mix.exs` file. If you are familiar with the Ruby ecosystem, this is similar to the [linline functionality provided by Bundler](https://bundler.io/guides/bundler_in_a_single_file_ruby_script.html).

To steal an example from the documentation, we can JSON-encode a map with [Jason](https://github.com/michalmuskala/jason).

```elixir
Mix.install([:jason])

IO.puts(Jason.encode!(%{hello: :world}))
```

When running this we see the following output:

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

Similar to the output of `mix deps.get`, we resolve, fetch, and compile our dependencies. Once `Mix.Install` is complete and we have our dependencies, we run our `IO.puts` and output the encoded JSON.

As a part of our first run our dependencies will be cached. As a result, subsequent runs (assuming the same version of Elixir and dependencies) will be much faster and not include dependency management:

```bash
› elixir mix_install_test.exs
{"hello":"world"}
```

To find out where the dependencies are bing cached on your system, you can pass the `verbose` option to `Mix.Install`.

```elixir
Mix.install(
  [:jason],
  verbose: true
)

IO.puts(Jason.encode!(%{hello: :world}))
```

Now, when we run our script it will tell us if found cached dependencies and where they are:

```bash
› elixir mix_install_test.exs
using /Users/me/Library/Caches/mix/installs/elixir-1.12.0-rc.1-erts-12.0/11989020f314102159a0c9ca882052fc

{"hello":"world"}
```

The dependency list passed to `Mix.Install` is th esame as your `deps` list in a project's `mix.exs`. This means you can take advantage of specifying versions and other [options](https://hexdocs.pm/mix/Mix.Tasks.Deps.html#module-options) provided by Mix.

As an example, when trying to create an example script that pulls from an API, I ran into an issue with OTP 24 in [mint](https://github.com/elixir-mint/mint) that [was resolved](https://github.com/elixir-mint/mint/pull/293) on their `main` branch, but not in a release. By leveraging the [git options](https://hexdocs.pm/mix/Mix.Tasks.Deps.html#module-git-options-git) provided by Mix, I was able to point at the `main` branch and get a working example. Since I was _actually_ using the Mint wrapper, [Mojito](https://github.com/appcues/mojito), I was also able to leverage the `override` option to tell Mix to use my overridden version of the dependency.

```elixir
Mix.install(
  [
    :jason,
    :mojito,
    # override version of Mint to include
    # https://github.com/elixir-mint/mint/pull/293
    {:mint, git: "https://github.com/elixir-mint/mint", branch: "main", override: true}
  ],
)
```

This shows that `Mix.install` was built to fully leverage the flexible dependency management provide in a full `mix` project`.

As I mentioned before, I got into the `Mix` configuration options because I wanted to make a mildly more complex option. One possible use case I imagined for these style of scripts was to provide an easy way to test out APIs. As an example, let's fetch the current price of Bitcoin from the [Coinbase API](https://developers.coinbase.com/).

```elixir
# Mix.Install from before
Mix.install(
  [
    :jason,
    :mojito,
    {:mint, git: "https://github.com/elixir-mint/mint", branch: "main", override: true}
  ],
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

We can now run this program like we would a Ruby or Python script.

```bash
› elixir bitcoin_price.exs
The current rate for Bitcoin is 55,277.6167

› elixir bitcoin_price.exs
The current rate for Bitcoin is 55,168.1100
```

This may not be the best way to explore new APIs, but gives us a new option. At the very least, we can now track our Bitcoin investment 💎🙌🚀!

We could also hand the script off to a co-worker if we wanted to. If we wanted to make some more portable and use it over time, we would probably want to consider locking down some of the versions we specify for our dependencies.

Because we still need Elixir installed on our system to run the script, this doesn't provide us with the portability of something like a go's ability to build executable binaries. For that, you may want to explore [Bakeware](https://github.com/bake-bake-bake/bakeware).

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

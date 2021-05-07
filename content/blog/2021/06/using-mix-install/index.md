---
title: "Using Mix.install"
date: "2021-06-26T06:31:13.265Z"
categories: ["elixir"]
---

Despite being a compiled language, Elixir has provided support for running code in a single file as though it were a scripting language since it's inception. By using the `.exs` extension, Elixir will compile the file in-memory and run the compiled code.

## A Basic Elixir Script

As a simple example we can create a file, `my_script.exs`, to print something "Hello, World"-like.

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

[This article](https://thinkingelixir.com/2019-04-running-an-elixir-file-as-a-script/) covers more of how this works as well as some of the downsides.

## Bringing in Dependencies

In order to further extend the ability for Elixir to be used in this scripting context, a new experimental feature has [been added](https://github.com/elixir-lang/elixir/pull/10674) in Elixir's 1.12 release, [`Mix.install`](https://hexdocs.pm/mix/1.12.0-rc.0/Mix.html#install/2). With `Mix.install`, you can list dependencies to use in your script like you would in a `mix.exs` file. If you are familiar with the Ruby ecosystem, this is similar to the [inline functionality provided by Bundler](https://bundler.io/guides/bundler_in_a_single_file_ruby_script.html).

## A Basic `Mix.install`

To start with something simple, let's steal an example from the documentation: JSON-encoding a map with the [Jason](https://github.com/michalmuskala/jason) package.

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

Similar to the output of `mix deps.get`, we resolve, fetch, and compile our dependencies. Once `Mix.install` is complete and we have our dependencies, we run our `IO.puts` and output the encoded JSON.

## Caching

As a part of our first run, our dependencies will be cached. This means subsequent runs will not need to include any dependency management:

```bash
› elixir mix_install_test.exs
# no resolving dependencies!
{"hello":"world"}
```

To find out where the dependencies are being cached on your system, you can pass the `verbose` option to `Mix.install`.

```elixir
Mix.install(
  [:jason],
  verbose: true
)
```

Now, when we run our script it will tell us if found cached dependencies and where they are:

```bash
› elixir mix_install_test.exs
using /Users/me/Library/Caches/mix/installs/elixir-1.12.0-rc.1-erts-12.0/11989020f314102159a0c9ca882052fc
```

Caching is based on a combination of Elixir and OTP versions as well as the dependencies you have listed. Changing any of these will result in a cache miss and require re-fetching and compiling packages.

## Mix Options

The dependency list passed to `Mix.install` is the same as your `deps` list in a project's `mix.exs`. This means you can take advantage of specifying versions and other [options](https://hexdocs.pm/mix/Mix.Tasks.Deps.html#module-options) provided by Mix.

When trying to create an example script that pulls from an API, I ran into an issue with using OTP 24 with [Mint](https://github.com/elixir-mint/mint). This issue [was resolved](https://github.com/elixir-mint/mint/pull/293) on their `main` branch, but not in a release. By leveraging the [git options](https://hexdocs.pm/mix/Mix.Tasks.Deps.html#module-git-options-git) provided by Mix, I was able to point at the `main` branch and get a working example. Since I was _actually_ using the Mint wrapper, [Mojito](https://github.com/appcues/mojito), I was also able to leverage the `override` option to tell Mix to use my overridden version of the dependency.

My resulting call to `Mix.install` ended up looking like the following:

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

This shows that `Mix.install` was built to fully leverage the flexible dependency management provided in a full `mix` project`.

In addition to the less commonly used options, there's also the ability to specify dependency versions. Pinning package versions can make it easier to share and re-use scripts in the future. Since scripts are often used for one-off tasks and less maintained, using the same package versions every time an help prevent the need to unnecessary maintenance.

## Another Example

I got into the `Mix` configuration options mentioned above because I wanted to make a mildly more complex option. One possible use case I imagined for these style of scripts was to provide an easy way to test out APIs. As an example, I wanted to take a ride on the Bitcoin hype train and fetch its current price from the [Coinbase API](https://developers.coinbase.com/).

```elixir
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

## Future

Because we still need Elixir installed on our system to run the script, this doesn't provide us with the portability of something like a [Go's ability to build executable binaries](https://www.digitalocean.com/community/tutorials/how-to-build-and-install-go-programs). For that, you may want to explore [Bakeware](https://github.com/bake-bake-bake/bakeware). For something between a single file script and `Bakeware`, you may also want to investigate [escript](https://hexdocs.pm/mix/master/Mix.Tasks.Escript.Build.html)s. With `escript` you can build your `mix` project into an executable. It does, however, require building the project for the architecture you are working with.

While you may need to reach for more robust options, `Mix.install` provides another useful tool in the Elixir toolbelt. With the addition of a single function, Elixir has greatly increased its abilities to write small scripts and be used in more ways.

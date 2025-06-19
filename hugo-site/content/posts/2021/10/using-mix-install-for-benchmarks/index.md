---
date: '2021-10-30T07:08:13'
tags:
- elixir
title: Using Mix.install for Benchmarks
---

In a [previous post](/2021/05/using-mix-install/), I covered using the [recently released](https://hexdocs.pm/elixir/1.12/changelog.html) function, [`Mix.install`](https://hexdocs.pm/mix/1.12/Mix.html#install/2). In this post, I cover how to use `Mix.install` for benchmarking.

If you are interested in (micro) benchmarking, you may already be familiar with [benchee](https://github.com/bencheeorg/benchee). Benchee provides the ability to collect and compare metrics about speed (via iterations per second) and memory usage for your function calls.

Because benchee is a third-party library, we have to install it via [Hex](https://hex.pm/). While benchee is useful enough to include it as a dev-dependency in your project, you may no longer _have_ to with `Mix.install`.

Let's take a look at an example ~~stolen~~ copied directly from the [benchee README](https://github.com/bencheeorg/benchee#benchee----)

```elixir
# my_test_benchmark.exs

Mix.install([:benchee])

list = Enum.to_list(1..10_000)
map_fun = fn i -> [i, i * i] end

Benchee.run(%{
  "flat_map" => fn -> Enum.flat_map(list, map_fun) end,
  "map.flatten" => fn -> list |> Enum.map(map_fun) |> List.flatten() end
})
```

With the addition of `Mix.install([:benchee])`, Elixir will fetch and build our benchee dependency on the first run.

```bash
› elixir my_test_benchmark.exs

Resolving Hex dependencies...
Dependency resolution completed:
New:
  benchee 1.0.1
  deep_merge 1.0.0
* Getting benchee (Hex package)
* Getting deep_merge (Hex package)
==> deep_merge
Compiling 2 files (.ex)
Generated deep_merge app
==> benchee
Compiling 39 files (.ex)
Generated benchee app
```

Once this is complete, we will see our benchee's benchmark results.

```bash
Operating System: macOS
CPU Information: Intel(R) Core(TM) i7-4771 CPU @ 3.50GHz
Number of Available Cores: 8
Available memory: 32 GB
Elixir 1.12.3
Erlang 24.1.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 0 ns
parallel: 1
inputs: none specified
Estimated total run time: 14 s

Benchmarking flat_map...
Benchmarking map.flatten...

Name                  ips        average  deviation         median         99th %
flat_map           2.50 K      400.75 μs    ±19.69%         378 μs      826.49 μs
map.flatten        1.24 K      806.81 μs    ±35.19%         658 μs        1625 μs

Comparison:
flat_map           2.50 K
map.flatten        1.24 K - 2.01x slower +406.05 μs
```

Because `Mix.install` caches our dependencies, subsequent runs will not need to rebuild the dependencies and immediately start the benchmark.

We now have a single file benchmark script that we can easily re-run and share with others.

A word of warning: a limitation of this approach is that you do not get access to your application code like you would when running a script via `mix run`. It may be a bit of a stretch, but this may be a feature and not a bug. By extracting the core functionality you want to benchmark away from the rest of your application, you may better avoid the noise and side effects that could come as a part of running other parts of your application.

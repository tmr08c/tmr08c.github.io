---
title: "Using Mix.install for Benchmarks"
date: "2021-08-06T14:08:13.265Z"
categories: ["elixir"]
---

In a previous post (ADD LINK), I discussed using [`Mix.install`](https://hexdocs.pm/mix/1.12/Mix.html#install/2) which was released as an experimental feature in Elixir [1.12](https://hexdocs.pm/elixir/1.12/changelog.html). In this post, I want to share a use case I have found for `Mix.install`: benchmarks.

If you are interested in (micro) benchmarking, you may already be familiar with [benchee](https://github.com/bencheeorg/benchee). Benchee provides the ability to collect and compare metrics about speed (via iterations per second) and memory usage for your function calls.

Because benchee is a third-party library, we have to install it via [hex](https://hex.pm/). Benchee is useful enough that it's useful to include it as a dev-dependency in your project, but, with `Mix.install`, you don't _have_ to. This can be useful for non-application specific benchmarks and providing a way to share your entire benchmark script with others.

Let's take a look at an example ~stolen~ copied directly from the [benchee README](https://github.com/bencheeorg/benchee#benchee----)

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

```
› elixir benchmark_example.exs

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

Once this is complete, we will see the normal benchee benchmark results.

```
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

Because `Mix.install` caches our dependencies, subsequent runs will not need to rebuild the dependencies and will immediately start the benchmark.

I began using this strategy when testing alternatives to a function that did not require complex, application-specific set up. I copied and pasted the function I was benchmarking into my benchmark script and started working on writing up alternative implementations. While I could have written my alternative implementations in the existing module and tested them in a separate benchamark script that I ran with `mix run`, I found that by creating a benchmark script that contained the implementations I was able to share the entire benchmark script as a part my pull request when explaining the change I made. This made it easier for coworkers to try to replicate my results.

A word of warning: a limitation of this approach is that you do not get your access to your application code like you would when running a script via `mix run`. It may be a bit of a stretch, but this may be a feature and not a bug. By having to pull the core functionality you want to benchmark away from the rest of your application, you may better avoid the noise and side effects that could come as a part of running other parts of your application.

# TODO

- [ ] previous post link

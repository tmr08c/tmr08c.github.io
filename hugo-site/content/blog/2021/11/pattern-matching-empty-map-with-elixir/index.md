---
date: '2021-11-30T17:31:13'
tags:
- elixir
title: Pattern Matching Empty Maps with Elixir
---

Unlike lists, using an empty map in pattern matching does not only match empty maps; it matches empty and populated maps.

```ex
> [] = [1,2,3]
** (MatchError) no match of right hand side value: [1, 2, 3]

> %{} = %{a: 1, b: 2, c: 3}
%{a: 1, b: 2, c: 3}
```

This was a bit of a surprise for me when attempting to match an empty list, so I wanted to share how you can pattern match an empty map (aka re-write the answers I found on [this StackOverflow post](https://stackoverflow.com/questions/33248816/pattern-match-function-against-empty-map)).

There appear to be two primary options for matching on an empty list.

1. You can use `==` and compare it to an empty map.

```ex
def my_fun(map) when map == %{}
```

2. You can use the [`map_size/1`](https://hexdocs.pm/elixir/1.12/Kernel.html#map_size/1) guard clause and compare it to `0`.

```ex
def my_fun(map) when map_size(map) == 0
```

I was curious if there was a performance implication to either choice, so I wrote a benchmark script to compare the options.

```ex
Mix.install([:benchee])

defmodule EmptyMapMatch do
  def using_size(map) when map_size(map) == 0, do: map
  def using_size(map), do: map
  def empty_map(map) when map == %{}, do: map
  def empty_map(map), do: map
end

Benchee.run(
  %{
    "checking size" => fn input -> EmptyMapMatch.using_size(input) end,
    "matching map" => fn input -> EmptyMapMatch.empty_map(input) end
  },
  inputs: %{
    "Empty" => %{},
    "Small" => Enum.zip(1..100, 1..100) |> Enum.into(%{}),
    "Medium" => Enum.zip(1..1_000, 1..1_000) |> Enum.into(%{}),
    "Large" => Enum.zip(1..10_000, 1..10_000) |> Enum.into(%{})
  },
  time: 10
)
```

This script will use [benchee](https://github.com/bencheeorg/benchee) to compare functions with `map_size` and `==` guard clauses on four different size maps.

After a few runs with different times, I found the options were equivalent speed-wise. Here is a sample result from a ten-second benchmark.

```
Operating System: macOS
CPU Information: Intel(R) Core(TM) i7-4771 CPU @ 3.50GHz
Number of Available Cores: 8
Available memory: 32 GB
Elixir 1.12.3
Erlang 24.1.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 10 s
memory time: 0 ns
parallel: 1
inputs: Empty, Large, Medium, Small
Estimated total run time: 1.60 min

Benchmarking checking size with input Empty...
Benchmarking checking size with input Large...
Benchmarking checking size with input Medium...
Benchmarking checking size with input Small...
Benchmarking matching map with input Empty...
Benchmarking matching map with input Large...
Benchmarking matching map with input Medium...
Benchmarking matching map with input Small...

##### With input Empty #####
Name                    ips        average  deviation         median         99th %
checking size        7.51 M      133.13 ns ±17314.76%           0 ns        1000 ns
matching map         7.43 M      134.52 ns ±17446.18%           0 ns        1000 ns

Comparison:
checking size        7.51 M
matching map         7.43 M - 1.01x slower +1.40 ns

##### With input Large #####
Name                    ips        average  deviation         median         99th %
checking size        7.66 M      130.53 ns  ±3435.02%           0 ns        1000 ns
matching map         7.58 M      131.93 ns  ±7672.54%           0 ns        1000 ns

Comparison:
checking size        7.66 M
matching map         7.58 M - 1.01x slower +1.40 ns

##### With input Medium #####
Name                    ips        average  deviation         median         99th %
checking size        7.14 M      140.04 ns ±22155.54%           0 ns        1000 ns
matching map         7.07 M      141.45 ns ±21487.37%           0 ns        1000 ns

Comparison:
checking size        7.14 M
matching map         7.07 M - 1.01x slower +1.41 ns

##### With input Small #####
Name                    ips        average  deviation         median         99th %
matching map         7.78 M      128.49 ns  ±3673.23%           0 ns        1000 ns
checking size        7.74 M      129.22 ns  ±3346.12%           0 ns        1000 ns

Comparison:
matching map         7.78 M
checking size        7.74 M - 1.01x slower +0.74 ns
```

These calls are both so quick that I would occasionally receive this warning from benchee:

> Warning: The function you are trying to benchmark is super fast, making measurements more unreliable!
> This holds especially true for memory measurements.
> See: https://github.com/PragTob/benchee/wiki/Benchee-Warnings#fast-execution-warning

I suspect that this explains why I would see the results flip-flop between runs.

I had expected `map_size` would be slower and slow down more as the input size increased, but the benchmarks show otherwise. If I had read the [documentation for `map_size`](https://hexdocs.pm/elixir/1.12/Kernel.html#map_size/1), I could have saved myself some time.

> This operation happens in constant time.

Since both options appear to be essentially equivalent, I plan to default to using `map_size` since it provides the flexibility to check for more than just empty maps.

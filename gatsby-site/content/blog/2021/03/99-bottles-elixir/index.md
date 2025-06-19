---
title:  "Singing 99 Bottles of Elixir"
date:   "2021-03-30T06:30:34.781Z"
categories: ["elixir"]
---

At work, we have begun reading through [_99 Bottles of OOP_](https://sandimetz.com/99bottles) as a book club book. The tongue-in-cheek tl;dr of the book, laid out in the [preface](https://sandimetz.com/99bottles-sample-ruby#preface), is:

> It turns out that everything you need to know about Object-Oriented Design (OOD) can be learned from the "[99 Bottles of Beer](https://en.wikipedia.org/wiki/99_Bottles_of_Beer)" song.

The authors use writing a program to "sing" the 99 Bottles of beer song to discuss different aspects of object-oriented programming and design. 

While it is not a book on functional programming, I wanted to see what I would come up with for a solution written in Elixir. I was also interested in how an Elixir solution would stand in terms of understandability when using the book's qualitative metrics. 

Everything discussed in this post was inspired by the [first chapter](https://sandimetz.com/99bottles-sample-ruby#chapter-rediscovering-simplicity) of the book, which is freely available as a reading sample. If this post has _anything_ that seems valuable, it is because of this book. Please consider reading the chapter and purchasing the book.

## The Solution 

Below is my attempt at solving the [99 Bottles](https://en.wikipedia.org/wiki/99_Bottles_of_Beer) problem using Elixir.

```elixir
defmodule NinetyNineElixirsOfJoy do
  def song, do: verses(99, 0)

  def verses(start, stop) do
    start..stop
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end

  def verse(number) do
    """
    #{String.capitalize(beverage(number))} on the wall, #{beverage(number)}.
    #{do_something(number)}
    """
  end

  defp beverage(0), do: "no more elixirs of joy"
  defp beverage(1), do: "1 elixir of joy"
  defp beverage(number), do: "#{number} elixirs of joy"

  defp do_something(0) do
    "Ask José to brew up some more, #{beverage(99)} on the wall."
  end

  defp do_something(1) do
    "Take it down and pass it around, #{beverage(0)} on the wall."
  end

  defp do_something(number) do
    "Take one down and pass it around, #{beverage(number - 1)} on the wall."
  end
end
```

To "sing" the whole song, you would call `song/0`. In turn, `song/0` calls out to `verses/2`, which calls and `join/2`s  `verse/1`s together. 

In addition to the public functions, we have helper functions, `beverage/1` and `do_something/1`

With `beverage/1`, I tried to be clever and replace "bottles of beer" with "elixirs of joy."

```elixir
defp beverage(0), do: "no more elixirs of joy"
defp beverage(1), do: "1 elixir of joy"
defp beverage(number), do: "#{number} elixirs of joy"
```

The primary value in splitting out this function was to handle the logic for pluralizing our elixir(s) of joy. While we could do something similar with `if` or `case`, I think pattern matching works well here.

Following the same idea, our generically named `do_something/1` function also leverages pattern matching. 

```elixir
defp do_something(0) do
  "Ask José to brew up some more, #{beverage(99)} on the wall."
end

defp do_something(1) do
  "Take it down and pass it around, #{beverage(0)} on the wall."
end

defp do_something(number) do
  "Take one down and pass it around, #{beverage(number - 1)} on the wall."
end
```

Our `do_something/1` function is similar when the number of bottles is between 1 and 99 - we take a bottle down and pass it around. However, when we are on the last bottle, we take "it" down instead of "one." 

Our final match for `do_something/1` is what to do when we run out of our beverage. In the song, the verse is usually about going to the store and buying some more. In our case, we will ask [José](https://github.com/josevalim), the alchemist that brought us the joyful Elixir language, to brew up some more of our elixirs of joy (the attempt at cleverness stops here).

Despite having a vague function name and some potential duplication, I think the solution ends up being straightforward to follow. Let's go back to _99 Bottles of OOP_ to help us evaluate this claim.

## Concrete or Abstract

The book posits that, when learning how to program, we start out writing concrete code. Over time, we often move towards writing abstract code as a default. While writing changeable code provides value, the book points out that it does not always make sense to start there.

> Unfortunately, abstractions are hard, and even with the best of intentions, it’s easy to get them wrong. Well-meaning programmers tend to over-anticipate abstractions, inferring them prematurely from incomplete information. Early abstractions are often not quite right, and therefore they create a catch-22. You can’t create the right abstraction until you fully understand the code, but the existence of the wrong abstraction may prevent you from ever doing so. This suggests that you should not reach for abstractions, but instead, you should resist them until they absolutely insist upon being created.

The idea of waiting to develop an abstraction until it "insists" upon being created has begun to shift my thinking. It's often tempting to "clean up" the code you are writing by seeking out abstractions. In my solution, I attempted to do so with the `beverage/1` and `do_something/1` functions. While I can cite the [rule of three](https://en.wikipedia.org/wiki/Rule_of_three_(computer_programming)) (consider refactoring when you do something three times), if I'm honest, I extracted those functions at the first sign of duplication.

The cost of abstractions is code that is generally more difficult to follow for the benefit of being easier to change. The inversion of this is that the benefit of concrete code is code that is easier to follow at the cost of being more difficult to change. Because we don't have any requirements to change the song, let's focus on how to decide if our code is optimized for understandability. 

## What is the code doing

> Code is easy to understand when it clearly reflects the problem it’s solving, and thus openly exposes that problem’s domain.

According to the book, one way to help identify where your code falls on the concrete-abstract spectrum is to see if a surface-level reading of the code can reveal what problem it is solving. The authors suggest asking questions that will reveal similarities and differences between code paths. When code is abstract, you often "hide" variations in your abstractions. As a result, identifying code path variations, at a glance,  will be more difficult with more abstract code. Below are questions that the authors suggest asking when evaluating a solution to the 99 Bottles problem.

1. How many verse variants are there?
1. Which verses are most alike? In what way?
1. Which verses are most different? In what way?
1. What is the rule to determine which verse should be sung next?

Let's attempt to answer these questions for our Elixir solution.

In both `beverage/1` and `do_something/1`, we use pattern matching to match on `0` and `1`, and then everything else (which is captured in `number`). Having this same pattern in both places may be an indication that we have three verse variants. 

Continuing with pattern matching as our guide, we can determine which verses are most similar and dissimilar. Because we have cases for `0` and `1`, we could say the final and penultimate verses are the most different (they have their own, special cases), and everything else is similar. The differences come from how we reference the beverage (if we look at `beverage/1`, we can see pluralization is at play) and what we do in the second line of the verse (usually this involves taking our beverage down, sometimes we ask for more). 

To understand how we determine which verse to sing next, we turn to the entry point, `song/0`. In `song/0` we iterate through the verses starting with the high number (99) and ending with the low number (0); this means the next verse will be based on the patterns that match `number - 1`.

As the name suggests, pattern matching has allowed us to recognize common patterns in our code. As a result, with a surface-level review of our solution, we have identified the high-level workflow for our 99 Bottles solution. We have also identified which code paths are similar and which are different. 

## A caveat

Based on our answers, the verse for when we have `2` elixirs of joy and `3` should essentially be identical (except the numbers). Let's see if this holds:

```{diff}
3 elixirs of joy on the wall, 3 elixirs of joy.
-Take one down and pass it around, 2 elixirs of joy on the wall.

2 elixirs of joy on the wall, 2 elixirs of joy.
+Take one down and pass it around, 1 elixir of joy on the wall.
```

With three elixirs of joy on the wall, when we take one down we still have two elixir**s** left. However, when we do the same after starting with two elixirs we only have one elixir (no **s**) left. This difference in remaining elixirs is the result of calling `beverage/1` with `number - 1` from `do_something/1`. 

Our call to `beverage/1` with `number - 1` makes it a little more complicated to answer the previous questions about the similarity between verses. We cannot simply look at the patterns we are matching on to know the number of verse variants. We now know there is another variant for when `number` is `2` - because `2 - 1` is `1`, and that will call a different variant of our `beverage/1` function (`beverage(1)`) than previous calls would have made (`beverage(number)`). 

Our matches for `beverage/1` and `do_something/1` still line up, but not as directly as we previously thought. For most cases (when `number` is greater than `2`), we end up matching the same `number` variant. However, with `2`, `1`, and `0`, we end up matching something different (`beverage(1)`, `beverage(0)`, and `beverage(number)`, respectively).

This slight mismatch "hides" the fact that we _actually_ have four verse variants:

|`number`|`do_something/1`|`beverage/1`|
|-|-|-|
|3+|`do_something(number)`|`beverage(number)`|
|2|`do_something(number)`|`beverage(1)`|
|1|`do_something(1)`|`beverage(0)`|
|0|`do_something(0)`|`beverage(number)`|

As discussed before, a concrete solution would directly surface all verse variants. Our "hidden" variant is an indication that our code may be more abstract than it is concrete. 

Is it bad that our code doesn't reveal all four verse variants directly? 

## It depends

As always, what is "good" or "bad" depends on your situation. As we said earlier, the concrete-abstract spectrum has tradeoffs on both sides - ease of understanding for ease of changeability.  

The authors suggest developers are often too quick to add abstractions to their solutions. Case in point, even after reading the chapter, my Elixir solution _still_ went for a more abstract solution, one that inadvertently hid some details about the variations of the 99 Bottles song. 

So, while not necessarily a bad solution, I may not have fully optimized the code on the side of understandability.  I could have instead started with a solution that is both [easier to write and understand](https://sandimetz.com/99bottles-sample-ruby#_shameless_green). 

A concrete solution may not be as "elegant," but it would be a lower-cost solution for a problem that didn't necessitate high effort. Even if I expected I would have upcoming changes, the authors have pointed out I am likely to pick the wrong abstraction. We always know less about our products now than we will later. By waiting until we know what functionality we _actually_ need, we increase our chances of discovering the right abstractions. 

One chapter in, and [99 Bottles of OOP](https://sandimetz.com/99bottles) has already begun influencing my perspective. I will leave you with a quote from the book:

> As programmers grow, they get better at solving challenging problems, and become comfortable with complexity. This higher level of comfort sometimes leads to the belief that complexity is inevitable, as if it’s the natural, inescapable state of all finished code. However, there’s something beyond complexity—a higher level of simplicity. Infinitely experienced programmers do not write infinitely complex code; they write code that’s blindingly simple.

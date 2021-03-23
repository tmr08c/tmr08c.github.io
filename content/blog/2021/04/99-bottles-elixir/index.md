---
title:  "Singing 99 Bottles of Elixir"
date:   "2021-02-26T06:30:34.781Z"
categories: ["elixir"]
---

At work, we have begun reading through [_99 Bottles of OOP_](https://sandimetz.com/99bottles) as a book club book. The tongue-in-cheek tl;dr of the book, laid out in the [preface](https://sandimetz.com/99bottles-sample-ruby#preface), is:

> It turns out that everything you need to know about Object-Oriented Design (OOD) can be learned from the "[99 Bottles of Beer](https://en.wikipedia.org/wiki/99_Bottles_of_Beer)" song.

The authors use writing a program to "sing" the 99 Bottles of beer song to discuss different aspects of object-oriented programming and design. 

While it's not a book on functional programming, I wanted to see what I would come up with for a solution written in Elixir. I also wanted to how the Elixir solution would compare in terms of understandability to the [possible solutions](https://sandimetz.com/99bottles-sample-ruby#section-c1-simplifying-code) discussed in the book.

Everything discussed in this post has been inspired by the [first chapter](https://sandimetz.com/99bottles-sample-ruby#chapter-rediscovering-simplicity) of the book. The first chapter is freely available as a reading sample. If this post has _anything_ that seems valuable, it is because of this book. Please consider reading the chapter and purchasing the book.

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

The primary value in splitting out the function was handling the logic for pluralizing our elixir(s) of joy. While we could do something similar with `if` or `case` in another language, I think pattern matching works well here.

Following the same idea, our generically named `do_something/1` function also leverages pattern matching. Having the same three pattern match cases (`0`, `1` and `number`) for `beverage/1` and `do_something/1` provides some potential value in making it easier to understand the pieces of the program (we will explore this further, later).

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

The idea of waiting to develop an abstraction until it "insists" upon being created has begun to shift my thinking. It's often tempting to "clean up" the code you are writing by seeking out abstractions. In my solution, I attempted to do so with the `beverage/1` and `do_something/1` functions. While I can cite the [rule of three](https://en.wikipedia.org/wiki/Rule_of_three_(computer_programming)#:~:text=Rule%20of%20three%20(%22Three%20strikes,be%20refactored%20to%20avoid%20duplication.) (consider refactoring when you do something three times), if I'm honest, I extracted those functions at the first sign of duplication.

The cost of abstractions is code that is generally more difficult to follow for the benefit of being easier to change. The inversion of this is that the benefit of concrete code is code that is easier to follow at the cost of being more difficult to change. Because we don't have any requirements to change the song, let's focus on how to decide if our code is understandable. 

## What is the code doing

> Code is easy to understand when it clearly reflects the problem it’s solving, and thus openly exposes that problem’s domain.

According to the book, one way to help identify where your code falls on the concrete-abstract spectrum is to see if a surface-level reading of the code can reveal what problem it is solving. The authors suggest asking questions that will reveal similarities and differences between code paths. When code is abstract, you often "hide" variations in your abstractions. As a result, identifying code path variations will be more difficult, at a glance, with more abstract code. Below are questions that the authors suggest asking when evaluating a solution to the 99 Bottles problem.

1. How many verse variants are there?
1. Which verses are most alike? In what way?
1. Which verses are most different? In what way?
1. What is the rule to determine which verse should be sung next?

Let's attempt to answer these questions for our Elixir solution.

Earlier, we hinted at how we may be able to determine the number of verse variants:  through our use of pattern matching. In both `beverage/1` and `do_something/1`, we match on `0` and `1`, and then everything else is captured in `number`; this may be an indication that we have three verse variants. 

Continuing with pattern matching as our guide, we can determine which verses are most similar and dissimilar. Because we have cases for `0` and `1`, we could say the final and penultimate verses are the most different (they have their own, special cases), and everything else is similar. The differences come from how we reference the beverage (if we look at `beverage/1`, we can see pluralization is at play) and what we do in the second line of the verse (usually this involves taking our beverage down, sometimes we ask for more). 

To understand how we determine which verse to sing next, we turn to the entry point, `song/0`, and its usage of `verse/1`. Looking at `verse/1` the current `number` is how we determine the verse to sing. In `song/0` we iterate through the verses starting with the high number (99) and ending with the low number (0). This means the next verse will be `number - 1`.

As the name suggests, pattern matching has allowed us to easily identify common patterns in our solution. With a surface-level review of our solution, we have recognized how 99 Bottles is sungl, and the similarities and differences between verses.

## A caveat

Based on our answers, the verse for when we have `2` elixirs of joy and `3` should essentially be identical (except the numbers). Let's see if this holds:

```{diff}
3 elixirs of joy on the wall, 3 elixirs of joy.
-Take one down and pass it around, 2 elixirs of joy on the wall.

2 elixirs of joy on the wall, 2 elixirs of joy.
+Take one down and pass it around, 1 elixir of joy on the wall.
```

With three elixirs of joy on the wall, when we take one down, we still have two elixir**s** left. However, when we do the same after starting with two elixirs we only have one elixir (no **s**) left. This difference in remaining elixirs is the result of calling `beverage/1` with `number - 1` in `do_something/1`. 

Our call to `beverage/1` with `number - 1` makes it a little more complicated to answer the previous questions about the similarity between verses. We cannot simply look at the patterns we are matching on to know the number of verse variants. We now know there is another variant for when `number` is `2` - because `2 - 1` is `1`, and that will call a different variant of our `beverage/1` function (`beverage(1)`) than previous calls would have made (`beverage(number)`). 

Our matches for `beverage/1` and `do_something/1` still line up, but not as directly as we originally thought. Rather than our verse matching `do_something(1)` to `beverage(1)`, we actually match `do_something(1)` to `beverage(1 - 1)`. For most cases (when `number` is greater than `2`), we end up matching the same `number` variant. However, with `2`, `1`, and `0`, we end up matching something different (`beverage(1)`, `beverage(0)`, and `beverage(number)`, respectively).

This slight mismatch "hides" the fact that we _actually_ have four verse variants:

1. `do_something(number)` with `beverage(number)` (when `number` is greater than `2`)
1. `do_something(number)` with `beveage(1)` (when `number` is `2`)
1. `do_something(1)` with `beverage(0)` (when `number` is `1`)
1. `do_something(0)` with `beverage(number)` (when `number` is `0`; `99` is passed into `beverage/1`, which is the `number` pattern)

Our "hidden" variant is an indication that our code may be more abstract than it is concrete. As discussed before, a more concrete version would more directly surface the four variants. It could look something like this:

```elixir
def verse(0) do
  """
  No more elixirs of joy on the wall, no more elixirs of joy.
  Ask José to brew up some more, 99 elixirs of joy on the wall.
  """
end

def verse(1) do
  """
  1 elixir of joy on the wall, 1 elixir of joy.
  Take one down and pass it around, no more elixirs of joy on the wall.
  """
end

def verse(2) do
  """
  2 elixirs of joy on the wall, 2 elixirs of joy.
  Take one down and pass it around, 1 elixir of joy on the wall.
  """
end

def verse(number) do
  """
  #{number} elixirs of joy on the wall, #{number} elixirs of joy.
  Take one down and pass it around, #{number - 1} elixirs of joy on the wall.
  """
end
```

While this example uses pattern matching, using `if` or `case` could also work.

Is it "bad" that our code doesn't reveal with four verse variants as directly? 

## It depends

As always, what is "right" or "best" depends on your situation. As we said earlier, the concrete-abstract spectrum has tradeoffs on both sides - ease of understanding versus ease of changeability.  

The authors suggest developers are often too quick to add abstractions to their solutions. Case in point, even after reading the chapter, my Elixir solution _still_ went for a more abstract solution, one that inadvertently hid some details about the variations of the 99 Bottles song. 

I could have instead started with a solution that is both easier to write and understand. It may not be as "elegant," but it would be a lower-cost solution for a problem that didn't necessitate high effort. Even if I expected I would have upcoming changes, the authors have pointed out I am more likely than not going to pick the wrong abstraction. We always know less about our products now than we will later. By waiting until we know what functionality we _actually_ need, we increase our chances of discovering the right abstractions. 

One chapter in, and [/99 Bottles of OOP/](https://sandimetz.com/99bottles) has already begun influencing my perspective. I will leave you with a quote from the book:

> As programmers grow, they get better at solving challenging problems, and become comfortable with complexity. This higher level of comfort sometimes leads to the belief that complexity is inevitable, as if it’s the natural, inescapable state of all finished code. However, there’s something beyond complexity—a higher level of simplicity. Infinitely experienced programmers do not write infinitely complex code; they write code that’s blindingly simple.

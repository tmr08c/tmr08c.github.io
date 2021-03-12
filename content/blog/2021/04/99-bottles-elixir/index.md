---
title:  "Singing 99 Bottles of Elixir"
date:   "2021-02-26T06:30:34.781Z"
categories: ["elixir"]
---

At work, we have begun reading through [_99 Bottles of OOP_](https://sandimetz.com/99bottles) as a bookclub book. The tongue-in-cheek tl;dr of the book laid out r), do: "#{number} elixirs of joy"

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

To "sing" the whole song, you would call `song/0`. This calls out to `verses/2`, which calls and `join`s  `verse/1` together. 

The choice of having functions for `song`, `verses`, and `verse` comes from the book. An advantage of having the three separate functions is ease of testing. It's easier to TDD starting with `verse/1`, moving onto `verses/2`, and ending with `song/0`.

In addition to the public functions, we have a few helper functions where we leverage pattern matching.

With `beverage/1`, I tried to be clever and replace "bottles of beer" with "elixirs of joy".

```elixir
defp beverage(0), do: "no more elixirs of joy"
defp beverage(1), do: "1 elixir of joy"
defp beverage(number), do: "#{number} elixirs of joy"
```

This generically named `beverage` function _could_ be repurposed for beer, kombucha, or anything else. The primary value in splitting it out was handling the logic for pluralizing our elixir(s) of joy. While we could do something similar with `if` or `case` in another language, I think pattern matching does really well here.

Following the same idea, our generlically named `do_something/1` function also leverages pattern matching. Having the same three pattern match casese (`0`, `1` and `number`) for `beverage/1` and `do_something/1` provies some potential value in making it easier to understand what's going on, we'll get to that later.

This time, rather than handling pluralization we are handling two actions that are similar and a third that is very different. 

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

Our `do_something/1` function is very similar when the number of bottles is between 1 and 99 - we take a bottle down and pass it around. When we are on the last bottle we take "it" down instead of "one." The two function calls are so similar that we could probably find a way to handle them in the same function if we wanted.

Our final match for `do_something/1` is what to do when we run out of our beverage. In the song, the suggestion is to go to the store and buy some more. In our case, we will ask José, the alchemist that brought us the joyful Elixir language, to brew up some more of our elixirs of joy (fortunately, the attempt at cleverness stops here).

While the function name isn't very helpful and we have some potential duplication, I think this ends up being fairly straightforward to follow. Let's go back to _99 Bottles of OOP_ to help us evaluate this idea.

## Concrete or Abstract

The book describes code as being on a concrete-abstract spectrum. On the concrete side, code is generally easier to understand, but harder to change. On the other side, abstract code is generally more difficult to understand, with the intention of being easier to change. 

The book posits that when learning how to program we often start out writing code the skews on the concrete since of things. Over time, we generally move towards abstract by default. While being able to write code that is changeable often proves to be valuable, the book points out that it doesn't always make sense to start there.

> Unfortunately, abstractions are hard, and even with the best of intentions, it’s easy to get them wrong. Well-meaning programmers tend to over-anticipate abstractions, inferring them prematurely from incomplete information. Early abstractions are often not quite right, and therefore they create a catch-22. You can’t create the right abstraction until you fully understand the code, but the existence of the wrong abstraction may prevent you from ever doing so. This suggests that you should not reach for abstractions, but instead, you should resist them until they absolutely insist upon being created.

The idea of waiting to abstract until it "insists" upon being created has struck me. It's often tempting to "clean up" code you are writing and seek out abstractions. In my solution, I attempted to do so with the `beverage/1` and `do_something/1` functions. While I can cite the [rule of three](https://en.wikipedia.org/wiki/Rule_of_three_(computer_programming)#:~:text=Rule%20of%20three%20(%22Three%20strikes,be%20refactored%20to%20avoid%20duplication.) (refactor when you do something three times), if I'm being honest I extrated those functions at the first sign of duplication.

The cost of abstractions is code that is generally more difficult to follow for the benefit of being easier to change. The inversion of this is that the benefit of concrete code is that it's esier to follow at the cost of being more difficult to change. Because we don't have a need to change the song, let's focus on how to decide if our code is more understandable. 

## What is the code doing

> Code is easy to understand when it clearly reflects the problem it’s solving, and thus openly exposes that problem’s domain.
> -- 99 Bottles of OOP

One way to help identify where your code falls on this spectrum is to see if a surface level review of the code can reveal what problem the code is mean to solve. In the book, the authors ask questions intended to reveal if you can easily see how similar or different different code paths would be. When code is abstract, you often "hide" variations in your abstations. As a result, more abstract code will often be more difficult to, at a glance, identify obvious code path variations. For the 99 Bottles problem, below are some questions the authors suggest asking to evaluate your solution.

1. How many verse variants are there?
1. Which verses are most alike? In what way?
1. Which verses are most different? In what way?
1. What is the rule to determine which verse should be sung next?

Let's attempt to answer these questions for our Elixir solution.

Earlier, we hinted at how we may determine the number of verse variants - through our use of pattern matching. In both `beverage` and `do_something` we match on `0` and `1`, and then everything else is captured in `number`. This may be an indicatation that we have three verse variants. 

Continuing on with pattern matching as our guide, we can try to determine which verses are most similar and disimilar. Because we have special matching for `0` and `1`, we could say the final and penultimate verses are the most different (they have their own special cases) and everything else is similar. The difference come from how we reference the beverage (if we look at `beverage/1`, we can see pluralization is at play) and what we do in the second line of the verse (usually this involves taking our beverage down, sometimes we ask for more). 

To understand how we determine which verse to sing next, we turn to the main entry point, `song/0`, and its usage of `verse/1`. Looking at `verse/1` the current `number` is how we determine the verse to sing. In `song/0` we iterate through the verses start with the high number and ending with the low number. This means the next verse will be `number - 1`.

Through pattern matching, we have surfaced variation on our codepaths, while allowing each individual function to only focus on its own case.

### A caveat

Based on our answers, the verse for when we have `2` elixirs of joy and `3` should basically be identical (except the numbers). Let's see if this holds:

```{diff}
3 elixirs of joy on the wall, 3 elixirs of joy.
-Take one down and pass it around, 2 elixirs of joy on the wall.

2 elixirs of joy on the wall, 2 elixirs of joy.
+Take one down and pass it around, 1 elixir of joy on the wall.
```

When we have three elixirs of joy on the wall, when we take one down, we still have two elixir**s**. However, when we do the same after starting with two elixirs we only have one elixir (no **s**) left. This is the result of calling `beverage/1` with `number - 1` in `do_something/1`. 

Our call to `beverage/1` with `number - 1` makes it a little more complicated to answer the pervious questions about similarity between verses. We cannot simply look at the patterns we are matching on to know the number of verse variants. We now know there is another variant for when `number` is `2` - because `2 - 1` is `1`, and that will call a different variant of our `beverage/1` function (`beverage(1)`), than previous calls would have made. 

Our matches for `beverage/1` and `do_something/1` still line up, but not as directly as we originally thought. Rather than our verse matching `do_something(1)` to `beverage(1)`, we actually match `do_something(1)` to `beverage(1 - 1)`. For most cases (when `number` is greater than `2`), we end up matching the same `number` variant. However, with `2`, `1`, and `0` we end up matching something different (`beverage(1)`, `beverage(0)`, and `beveage(number)`, respectively).

This slight mismatch "hides" the fact that we have four verse variants:

1. `do_something(number)` with `beverage(number)` (when `number` is greater than `2`)
1. `do_something(number)` with `beveage(1)` (when `number` is `2`)
1. `do_something(1)` with `beverage(0)` (when `number` is `1`)
1. `do_something(0)` with `beverage(number)` (when `number` is `0`; `99` is passed into `beverage/1`, which is the `number` pattern)

This is an indication that our code may be more abstract than it is concrete. A more concrete version would more directly surface the four variants. It could look something like this:

```elixir
def verse(0) do
  """
  No more elixirs of joy on the wall, no more elixirs of joy.
  Ask Jose to brew up some more, 99 elixirs of joy on the wall.
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

While this example uses pattern matching, `if` or `case` could also work.

Is it "bad" that our code doesn't reveal with four verse variants as directly? 

## It depends

As always, what is "right" or "best" depends on your situation. As we said earlier, the concrete-abstract spectrum has tradeoffs on both sides - ease of understanding for ease of changeability.  

In the case of this problem, there are no upcoming feature requests I am expecting, so optimizing for change may be the wrong tradeoff. If I could develop a solution that was easier to write and is easier to understand by others, that sounds like a win. Even if I expected I would have upcoming changes, it would probably be better to wait unti I need to make the actual changes before I attempt generating abstractions based on assumptions.

The authors suggest developers are often too quick to add abstractions to their solutions. This comes at the cost of making the code harder to understand, and, possibly, developing against the wrong abstractions. Even after reading the chapter, my Elixir solution still went for a more abstract solution, one that inadvertently hid some details about how the varations of the 99 Bottles song. While I feel like the solution I came up with is a nice mix of concrete and abstract, and I am happy with how it turned out, but I think I am suffering from "it's my code, so I think it's great" syndrome. Trying to see things through a more pragmatic lens (and heavily influenced by conclusions drawn in the book), I wonder if the addiction to multiple, small functions needs should be continued for new solutions. I will never no less about the solution than I do right now, so maybe I should just wait until I know more before I make more abstract assumptions.

One chapter in, and [/99 Bottles of OOP/](https://sandimetz.com/99bottles) already feels like it will be taking me to a new stage in my programming life. I will leave you with a quote from the book:

> As programmers grow, they get better at solving challenging problems, and become comfortable with complexity. This higher level of comfort sometimes leads to the belief that complexity is inevitable, as if it’s the natural, inescapable state of all finished code. However, there’s something beyond complexity—a higher level of simplicity. Infinitely experienced programmers do not write infinitely complex code; they write code that’s blindingly simple.



----

* https://sandimetz.com/99bottles



```elixir
  test "verse/1" do
    assert NinetyNineElixirsOfJoy.verse(99) == """
           99 elixirs of joy on the wall, 99 elixirs of joy.
           Take one down and pass it around, 98 elixirs of joy on the wall.
           """

    assert NinetyNineElixirsOfJoy.verse(2) == """
           2 elixirs of joy on the wall, 2 elixirs of joy.
           Take one down and pass it around, 1 elixir of joy on the wall.
           """

    assert NinetyNineElixirsOfJoy.verse(1) == """
           1 elixir of joy on the wall, 1 elixir of joy.
           Take one down and pass it around, no more elixirs of joy on the wall.
           """

    assert NinetyNineElixirsOfJoy.verse(0) == """
           No more elixirs of joy on the wall, no more elixirs of joy.
           Ask Jose to brew up some more, 99 elixirs of joy on the wall.
           """
  end
```

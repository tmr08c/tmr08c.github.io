---
title:  "Singing 99 Bottles of Elixir"
date:   "2021-02-26T06:30:34.781Z"
categories: ["elixir"]
---

At work, we have begun reading through [_99 Bottles of OOP_](https://sandimetz.com/99bottles) as a bookclub book. The tongue-in-cheek tl;dr of the book laid out in the [preface](https://sandimetz.com/99bottles-sample-ruby#preface) is:

> It turns out that everything you need to know about Object-Oriented Design (OOD) can be learned from the "[99 Bottles of Beer](https://en.wikipedia.org/wiki/99_Bottles_of_Beer)" song.

Over the course of the book, the authors use the fairly simple problem of writing a program to "sing" the 99 Bottles of beer song to discuss different aspect of Object-Oriented Programming and Design. Our group is a chapter in and I am already excited about the disucssions we are beginning to have. 

In the first chapter the authors discuss the balance between concrete and abstact code and how that ties to easy to understand and easy to change code respectively. They then show a few [possible solutions](https://sandimetz.com/99bottles-sample-ruby#section-c1-simplifying-code) to the 99 Bottles Problem, and provide measures on which to to [judge the solutions](https://sandimetz.com/99bottles-sample-ruby#section-c1-judging-code).

While I like the objective metrics that were discussed, the suggestions around evaluating if code is easy to understand stuck out to me.

> Code is easy to understand when it clearly reflects the problem it’s solving, and thus openly exposes that problem’s domain.

To determine if the code reflects the problem, you will need to ask yourself questions that are specific to the problem. In the case of the 99 Bottles problem, the authors suggestions the following questions:

1. How many verse variants are there?
1. Which verses are most alike? In what way?
1. Which verses are most different, and in what way?
1. What is the rule to determine which verse comes next?

While it's not a book on functional programming, I wanted to see how a solution in Elixir would stand up against these questions. 





----

* https://sandimetz.com/99bottles


```elixir
defmodule NientyNineElixirsOfJoy do
  @moduledoc """
  Documentation for `NientyNineElixirsOfJoy`.
  """

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
    "Ask Jose to brew up some more, #{beverage(99)} on the wall."
  end

  defp do_something(number) do
    "Take one down and pass it around, #{beverage(number - 1)} on the wall."
  end
end
```

```elixir
  test "verse/1" do
    assert NientyNineElixirsOfJoy.verse(99) == """
           99 elixirs of joy on the wall, 99 elixirs of joy.
           Take one down and pass it around, 98 elixirs of joy on the wall.
           """

    assert NientyNineElixirsOfJoy.verse(2) == """
           2 elixirs of joy on the wall, 2 elixirs of joy.
           Take one down and pass it around, 1 elixir of joy on the wall.
           """

    assert NientyNineElixirsOfJoy.verse(1) == """
           1 elixir of joy on the wall, 1 elixir of joy.
           Take one down and pass it around, no more elixirs of joy on the wall.
           """

    assert NientyNineElixirsOfJoy.verse(0) == """
           No more elixirs of joy on the wall, no more elixirs of joy.
           Ask Jose to brew up some more, 99 elixirs of joy on the wall.
           """
  end
```

---
title: "Adding Randomness to FactoryBot"
date: "2021-04-20T06:14:13.265Z"
categories: ["testing", "ruby"]
---

While tests help improve the robustness of your system, you are limited by the test cases and sample data used in your test suite. In this post, we will cover how to add randomness to your test as a way to reduce some of these limitations. The examples in the post will be using [FactoryBot](https://github.com/thoughtbot/factory_bot), but you should be able to apply similar concepts to [Rails' fixtures](https://guides.rubyonrails.org/testing.html#the-low-down-on-fixtures) and other testing tools as well.

## Sequences

### Inline

A commonly used form of randomness in FactoryBot is [sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences). When defining a factory, you can use the `sequence` method to have a block of code run each time a particular attribute needs to be generated. These are known as [inline sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inline-sequences).

A common use case for inline sequences is to avoid issues with uniqueness constraints. For example, if a `User` record requires a unique `username`, then you can leverage a sequence to append an ever-increasing number to the end of it.

```ruby
factory :user
  usename { "user-#{n}" }
end
```

Now, anytime we create a user, we will have a unique `username` attribute:

```ruby
3.times { puts FactoryBot.build(:user) }

=> <User @username="user-1">
=> <User @username="user-2">
=> <User @username="user-3">
```

### Global

FactoryBot also supports [global sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences). These are named sequences that can be used in other factories. Global sequences can provide globally unique, valid data across your factories (e.g., email addresses) and also be an easy way to generate new data in general. For example, at work, we have a `string` sequence that returns the string, "string" with a number appended.

```ruby
sequence :string do |n|
  "string #{n}"
end

factory :user do
  first_name { generate(:string) }
  last_name { generate(:string) }
end

=> <User @first_name="string 1", @last_name="string 2">
=> <User @first_name="string 3", @last_name="string 4">
=> <User @first_name="string 5", @last_name="string 6">
```

While this does not provide realistic data, it does provide an indication to the team that we don't care about the value of an attribute and just want a string value.

### Randomness

Sequences provide an easy way to introduce randomness into your system. Every time your tests run in a different order, your factory has a chance of generating a value at a different step in the sequence, resulting in slightly different attributes. This small amount of variation provides an easy starting point for introducing randomness into your system but is limited. Let's explore a few other options that introduce more variation in our values.

## Sampling Options

Another form of randomness I like to use comes into play for attributes that have a set number of valid values (i.e., `enum`s). Rather than having your factory always set the attribute with the same value, consider having the value chosen randomly each time you create a factory.

Let's imagine we have a `class` representing a pizza, and that pizza can have toppings. Our restaurant only has a set number of toppings, and we have a constant that lists what those are.

```ruby
class Pizza
  AVAILABLE_TOPPINGS = %w[
    bacon
    jalapenos
    onions
    pepperoni
    peppers
    pineapple
  ]

  attr_writer :toppings

  def initalize(toppings)
    @toppings = Array(toppings)
  end
end
```

In our factory, we can choose a random entry from our list of available toppings using Ruby's [`Array#sample`](https://ruby-doc.org/core/Array.html#method-i-sample) method.

```ruby
FactoryBot.define do
  factory :pizza do
    toppings { Pizza::AVAILABLE_TOPPINGS.sample }
  end
end
```

This results in a different topping being chosen every time we use our factory to create a new pizza.

```ruby
3.times { puts FactoryBot.build(:pizza).inspect }

=> <Pizza @toppings=["jalapenos"]>
=> <Pizza @toppings=["onions"]>
=> <Pizza @toppings=["pineapple"]>
```

### Random Amount of Random

Often, when dealing with `enum`-like data, your attribute will only be a single value, and `Array#sample` is all you will need. However, in our example, we can have no toppings, one topping, or many toppings. Currently, our current factory is limited to always give us a `Pizza` with a single topping.

Herein lies an issue with example-based testing - it relies on the examples you remember to include. We may think to test zero, one, and many toppings in a test case specifically dealing with presenting the list of toppings, but we may not think about it when writing more complicated higher-level tests. What if, instead, the factory always returned a random number of random toppings? This would add some variability to **all** tests that use this factory in our system.

We can update the `toppings` attribute in our factory to build a random number of pizza toppings - some number between no toppings and the total number of available toppings. We can represent this with something like `Array.new(rand(0..Pizza::AVAILABLE_TOPPINGS.size))`. This will generate a random number between zero and the number of available toppings (`rand(0..Pizza::AVAILABLE_TOPPINGS.size)`) and create an `Array` of that size. We can leverage [`Array#new`](https://ruby-doc.org/core/Array.html#method-c-new)'s block argument to run some code to generate each entry in our new array. For generating entries, we will use the same code we had before, `Pizza::AVAILABLE_TOPPINGS.sample`. Putting this all together, our factory looks like the following:

```ruby
FactoryBot.define do
  factory :pizza do
    toppings do
      Array.new(rand(0..Pizza::AVAILABLE_TOPPINGS.size)) do
        Pizza::AVAILABLE_TOPPINGS.sample
      end
    end
  end
end
```

Now, when we generate our `Pizza`s, we will have a different number of different toppings.

```ruby
3.times { puts FactoryBot.build(:pizza).inspect }

<Pizza @toppings=["peppers", "bacon"]>
<Pizza @toppings=["peppers"]>
<Pizza @toppings=["pepperoni", "pineapaple", "bacon", "onions"]>
```

### Valid Amount of Random

Something to be careful of when dealing with randomness is the fact you could end up with invalid data. Our current factory doesn't do anything to protect against having multiple entries of the same topping. Below is an example of what this could look like.

```ruby
3.times { puts FactoryBot.build(:pizza).inspect }

<Pizza @toppings=["peppers", "peppers", "bacon", "peppers", "peppers", "onions"]>
<Pizza @toppings=["peppers", "peppers"]>
<Pizza @toppings=["onions", "bacon", "peppers", "jalapenos", "pineapaple"]>
```

Depending on your model, this may or may not matter (maybe people can order a quadruple serving of peppers if they want). Be warned that randomness isn't a silver bullet for catching validation errors. While adding randomness to your tests can result in invalid cases, it's not always the case that your application will result in a test failure when there is invalid data.

In our case, we may validate the uniqueness of toppings in another test and want our factory to result in a valid record. To do this, we can update how we create the toppings list in our factory.

```ruby
FactoryBot.define do
  factory :pizza do
    toppings do
      # Set will make sure we have unique list
      topping_set = Set.new
      topping_count = rand(0..Pizza::AVAILABLE_TOPPINGS.size)

      while topping_set.size < topping_count
        topping_set << Pizza::AVAILABLE_TOPPINGS.sample
      end

      topping_set.to_a
    end
  end
end
```

```ruby
3.times { puts FactoryBot.build(:pizza).inspect }

<Pizza @toppings=["onions", "peppers", "pineapaple", "bacon"]>
<Pizza @toppings=["pineapaple", "peppers", "bacon", "pepperoni", "onions", "jalapenos"]>
<Pizza @toppings=[]>
```

## Delegating Randomness

The final suggestion for introducing randomness into your system relies on a gem, [Faker](https://github.com/faker-ruby/faker). Faker provides hundred of modules themed around different topics on which you can get fake data.

With Faker, you can get names, email addresses, physical addresses, quotes, and more.

```ruby
Faker::Name.name
=> "Marquerite Legros CPA"

Faker::Internet.email
=> "lawana_lesch@donnelly.com"

Faker::TvShows::TheITCrowd.quote
=> "Well, I'm the boss... Head Honcho. El Numero Uno. Mr. Big. The Godfather. Lord of the Rings. The Bourne... Identity. Er... Taxi Driver. Jaws. I forgot the question quite a while back. Who are you, again?"

Faker::TvShows::SiliconValley.url
=> "http://raviga.com"
```

Faker provides an advantage over our [sequences](#sequences) example in that you aren't pulling from the same basic template. Instead, you are pulling from a large list of possible values. Even better, most basic data types can generate fairly complex sample data. For example, looking at how [names](https://github.com/faker-ruby/faker/blob/master/lib/locales/en/name.yml) can be generated, there are prefixes, suffixes, and middle names; these additional variations could help potentially catch issues if you were not handling them.

Like all of our options so far, Faker does not provide the same level of scrutiny you would get from a property testing tool. Its goal is to provide realistic data, so you will not have test data that looks like someone rolled their head on the keyboard. That said, its realistic data is likely to provide more variation and randomness than you would if left to your own devices.

## Conclusion

Adding some randomness into your tests can potentially help catch use cases that you were not thinking of when designing a solution. While there are different levels of randomness and chances of catching corner cases in your system, some of the options covered in this post are low-cost enough that I use them by default.
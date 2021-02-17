---

title: 'Adding Randomness to FactoryBot'
date: '2021-04-20T06:14:13.265Z'
categories: ['testing', 'ruby']
---

Adding randomness to your tests can help you to build a more robust system. While this is true of tests in general, tests are limited by the test cases and sample data that you come up with. Adding randomness into your tests can help avoid these limitations. In this post, we will cover a few easy ways to add randomness to sample records in your tests. While this isn't as robust as the idea of [propery-based testing](https://medium.com/criteo-engineering/introduction-to-property-based-testing-f5236229d237) introduced by [QuickCheck](https://users.cs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf), I find it can be a low-cost step in the right direction. The examples in the post will be centered around [FactoryBot](https://github.com/thoughtbot/factory_bot), but you should be able to apply them to [fixtures](https://guides.rubyonrails.org/testing.html#the-low-down-on-fixtures) or other testing strategies as well.

## Sequences

### Inline

A commonly used form of randomness in FactoryBot is [sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences). When defining a factory, you can use the `sequence` method to have a block of code run each time a particular attribute needs to be generated. These are known as [inline sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inline-sequences).

A common use case for inline sequences is avoid issues with uniqueness constraints. For example, if a `User` record requires unique `username`s, then you can leverage a sequence to append an ever-increasing number to the end of it.

```ruby
factory :user
  usename { "user-#{n}" }
end
```

Now, anytime we create a user, we will have a unique `username` attribute:

```ruby
3.times { puts FactoryBot.build(:user) }

=> <User username="user-1">
=> <User username="user-2">
=> <User username="user-3">
```

### Global

FactoryBot also supports [global sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences). These are named sequences that can be used in other factories. Global sequences can be useful for providing globally unique, valid data across your factories (e.g., email addresses), but they can also be an easy way to generate new values without manually setting them. For example, at work, we have a `string` sequence that returns the string, "string" with a number appended.

```ruby
sequence :string do |n|
  "string #{n}"
end

factory :user do
  sequence(:username) { |n| "user-#{n}" }
  first_name { generate(:string) }
  last_name { generate(:string) }
end

=> <User username="user-1", first_name="string 1", last_name="string 2">
=> <User username="user-2", first_name="string 3", last_name="string 4">
=> <User username="user-3", first_name="string 5", last_name="string 6">
```

### Randomness

While sequences generate similarly shaped data and aren't going to cover any particularly difficult-to-process data they do introduce randomness into your system. Every time your tets run in a different order, your factory has a chance of generating a value at a different step in the sequence and end up with a slightly different value. This small amount of variation provides an easy starting point for introducing randomness into your system.




* Examples
  * Sequences (built into FactoryBot)
    * Global sequences (`generate`)  https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences
    * Inline https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inline-sequences
  * Array.sample
  * Faker https://github.com/faker-ruby/faker
    * May need to use https://github.com/faker-ruby/faker#ensuring-unique-values
        * We've found flaky tests as the result of `.not contain` faker data that is found elsewhere
    * Can be fun, but also add noise if people use different sets across different tests

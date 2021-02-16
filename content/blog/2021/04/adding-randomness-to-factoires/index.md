---
title: 'Adding Randomness to FactoryBot'
date: '2021-04-20T06:14:13.265Z'
categories: ['testing', 'ruby']
---

Adding randomness to your tests can help you to build a more robust system. While this is true of tests in general, tests are limited by the test cases and sample data that you come up with. Adding randomness into your tests can help avoid these limitations. In this post, we will cover a few easy ways to add randomness to sample records in your tests. While this isn't as robust as the idea of [propery-based testing](https://medium.com/criteo-engineering/introduction-to-property-based-testing-f5236229d237) introduced by [QuickCheck](https://users.cs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf), I find it can be a low-cost step in the right direction. The examples in the post will be centered around [FactoryBot](https://github.com/thoughtbot/factory_bot), but you should be able to apply them to [fixtures](https://guides.rubyonrails.org/testing.html#the-low-down-on-fixtures) or other testing strategies as well.

## Sequences

A commonly used form of randomness in FactoryBot is [sequences](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences). When defining a factory, you can use the `sequence` method to have a block of code run each time a particular attribute needs to be generated.

```ruby
factory :phone
  serial_number { |n| "phn-#{n}" }
end
```


* Examples
  * Sequences (built into FactoryBot)
    * Global sequences (`generate`)  https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences
    * Inline https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inline-sequences
  * Array.sample
  * Faker https://github.com/faker-ruby/faker
    * May need to use https://github.com/faker-ruby/faker#ensuring-unique-values
        * We've found flaky tests as the result of `.not contain` faker data that is found elsewhere
    * Can be fun, but also add noise if people use different sets across different tests

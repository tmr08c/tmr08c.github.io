---
title: 'Adding Randomness to FactoryBot'
date: '2021-04-20T06:14:13.265Z'
categories: ['testing', 'ruby']
---

Adding randomness to your tests can help you to build a more robust system. While this is true of tests in general, tests are be limited by the test cases and sample data that you come up with. Adding randomness into your tests can prevent building solutions limited to the data set you come up with. While this isn't as robust as the idea of [propery-based testing](https://medium.com/criteo-engineering/introduction-to-property-based-testing-f5236229d237) defined by [QuickCheck](https://users.cs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf), I find it can be a low-cost step in the right direction.

In this post, we will introduce a few ways to add randomness into yours tests through the factory data that you create.

* Advantages of randomness
  * Not as far as something like https://medium.com/criteo-engineering/introduction-to-property-based-testing-f5236229d237
* Examples
  * Sequences (built into FactoryBot)
    * Global sequences (`generate`)  https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#global-sequences
    * Inline https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inline-sequences
  * Array.sample
  * Faker https://github.com/faker-ruby/faker
    * May need to use https://github.com/faker-ruby/faker#ensuring-unique-values
        * We've found flaky tests as the result of `.not contain` faker data that is found elsewhere
    * Can be fun, but also add noise if people use different sets across different tests

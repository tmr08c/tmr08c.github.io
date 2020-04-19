---
title: 'Concurrent Ruby - Hello, Async'
date: '2020-04-19T00:36:13.265Z'
categories: ['ruby', 'concurrency']
---

Over the last few years I have been playing on-and-off with [Elixir](https://elixir-lang.org/) and have **loved** it. One of the reasons there is so much hype around Elixir is the out-of-the-box support for writing highly concurrent code. The concurrency model is a native part of the language that comes from the [Erlang](https://www.erlang.org/) VM on which Elixir runs. 

I have been getting back into writing code on a daily basis, and have been working in Ruby again. After so much focus on writing concurrent code, I want to see if I can avoid giving that up when I am in Ruby. I know I will not get near what is supported in Elixir, but that doesn't mean I should avoid concurrency altogether!

One option I found, that happened to already be including in my project at work is the [`concurrent-ruby` gem](https://github.com/ruby-concurrency/concurrent-ruby). From the gem's description:

> Modern concurrency tools including agents, futures, promises, thread pools, supervisors, and more. Inspired by Erlang, Clojure, Scala, Go, Java, JavaScript, and classic concurrency patterns.

Having multiple concurrency options available seems like a great way to pick the best tool for the job and learn **a lot** along the way.

## Choosing where to start 

Since there are so many concurrency models available through this gem, I thought a good place to start would be to pick a model and write some code. The first option [in the list](https://github.com/ruby-concurrency/concurrent-ruby#general-purpose-concurrency-abstractions), [`Async`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html) happens to also be inspired by Erlang's `gen_server`, so I thought that would be a good place to start. 

  * chose `Async` because loosely based on Erlang's `gen_server`
    * there is mor feature rich Actor (confirm: I think it's still under active development)
## Understanding how it works
  * Making sample class

    * Copied exapmle from docs, but added `sleep` to see the impact 

      ```ruby
      class Hello
        include Concurrent::Async

        # @return [Concurrent::IVar]
        def hello(name)
          sleep(3)
          puts "Hello, #{name}"
        end
      end
      ```
  * Made CLI for interacting
  * Basics - call with `async` call with `await` 
  * When are threads made
    * adding thread counter to CLI
    * trying with `.new`
    * `.new` and calling method (works), uses same thread for same variable because it's copying the actor model and queue 
    * can make new instances and call method everytime to make it work
    * lazy thread creation - eventually get to https://github.com/ruby-concurrency/concurrent-ruby/blob/7dc6eb04142f008ffa79a59c125669c6fcbb85a8/lib/concurrent-ruby/concurrent/executor/ruby_executor_service.rb#L17-L25
## conclusion
  * just scratching surface of libaray 
  * enjoying sleep learning - try something, poke around code, no rush to build
  * link to code

# TODO
- [x] add commands to test.rb that re-uses variable and one that creates new
- [x] create README in demo app
- [x] create repo for demo app

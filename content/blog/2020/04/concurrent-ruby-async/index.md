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

In Elixir/Erlang, a `gen_server` would run in a new Erlang process. This allows it to run independently of the caller's process. `concurrent-ruby` provides similar functionality with Ruby's [`Thread`](https://ruby-doc.org/core-2.7.0/Thread.html). When `include`d in a class, `Async` adds two new methods to the class that can be used as proxies between calling methods you define in the class, but will have to class run its code in a separte thread. The new methods are (1) `async` which will immediately return to the caller while continuing the work in the new thread and (2) `await` which will also do the work in the other thread, but, when called, will block (or (a)wait in) the main thread until the method called finishes and returns. _Note: if you (and your team) are familiar with the Erlang terms, you can also use `cast` (`async`) and `call` (`await`)._

## Starting with an example

I decided to start with the example they have [in the docs](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html):

```ruby
require 'concurrent-ruby'

class Hello
  include Concurrent::Async

  def hello(name)
    "Hello, #{name}!"
  end
end
```

Starting out in `irb` I was able to make the following calls: 


```ruby
# async version
irb(main):009:0> Hello.new.async.hello("world")
=> #<Concurrent::IVar:0x00007fe09b08b230 @__Lock__=#<Thread::Mutex:0x00007fe09b08b1b8>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b190>, @event=#<Concurrent::Event:0x00007fe09b08b118 @__Lock__=#<Thread::Mutex:0x00007fe09b08b0a0>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b078>, @set=false, @iteration=0>, @reason=nil, @value=nil, @observers=#<Concurrent::Collection::CopyOnWriteObserverSet:0x00007fe09b08b028 @__Lock__=#<Thread::Mutex:0x00007fe09b08afd8>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08afb0>, @observers={}>, @dup_on_deref=nil, @freeze_on_deref=nil, @copy_on_deref=nil, @do_nothing_on_deref=true, @state=:pending>

# await version
irb(main):010:0> puts Hello.new.async.hello("world")
#<Concurrent::IVar:0x00007fe0ac2762f0>
```

Simply returning a string is fast enough that I didn't notice a spped difference between the two calls, but do notice a difference in what is returned. Proxing through `async` and `await` change the return type of the method to be a [`Concurrent::IVar`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/IVar.html). From the docs these are described as:

> An `IVar` is like a future that you can assign. As a future is a value that is being computed that you can wait on, an `IVar` is a value that is waiting to be assigned, that you can wait on. `IVars` are single assignment and deterministic.

My time working with JavaScript has me assum this is similar enough to a [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) that I can continue to focus my learning on the `Async` module and come back to `IVar`s at a later time.


At this point, things seem to be working since I am getting back `IVar`s, but it doesn't _perform_ different. Often times you would move work into another thread if it's slow and gets in the way of your main thread. To replicate this in my testing, I decided to add a [`sleep`](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-sleep). I also figured out a way to avoid thinkning about `IVars` during my exploration; I could `puts` in the method. This gives me a visual indication my methods are being called without having to focus on the response. Now, my test class looks something like:

```ruby
require 'concurrent-ruby'

class Hello
  include Concurrent::Async

  def hello(name)
    sleep(3) 
    puts "Hello, #{name}!"
  end
end
```

After reloading this class into `irb`, I now have a visual indication when the method has run from the `puts` statement, and the `sleep` delays the method run. 

For my call to `await` this `sleep` is blocking, so when I call the method, it's similar to calling it locally even though it's running in a separate thread. This means that I have to wait for the `sleep` to finish before I can do anything:

<img src='./hello-await-with-sleep.gif' lazy />

However, with `async`, the method returns right away and the thread continues to run on its own. This means I can continue to interact with the main `irb` thread while waiting for my result to print:

<img src='./hello-async-with-sleep.gif' lazy />


When loading this into `irb` again and calling our methods we can now see more of a pause:

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

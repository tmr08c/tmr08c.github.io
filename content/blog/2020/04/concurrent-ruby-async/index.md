---
title: 'Concurrent Ruby - Hello, Async'
date: '2020-04-19T00:36:13.265Z'
categories: ['ruby', 'concurrency']
---

Over the last few years, I have been fiddling on-and-off with [Elixir](https://elixir-lang.org/) and have **loved** it. One of the reasons there is so much hype around Elixir is the [out-of-the-box support for writing highly concurrent code](https://elixir-lang.org/getting-started/processes.html). The concurrency model is a native part of the language that comes from the [Erlang](https://www.erlang.org/) VM on which Elixir runs. 

I have been getting back into writing code on a daily basis, and have been working in Ruby again. After so much focus on writing concurrent code, I want to see if I can avoid giving that up when I am in Ruby. I know I will not get near what is supported in Elixir, but that doesn't mean I should avoid concurrency altogether!

One option I found, that happened to already be included in my project at work, is the [`concurrent-ruby` gem](https://github.com/ruby-concurrency/concurrent-ruby). From the gem's description:

> Modern concurrency tools including agents, futures, promises, thread pools, supervisors, and more. Inspired by Erlang, Clojure, Scala, Go, Java, JavaScript, and classic concurrency patterns.

Having multiple concurrency options available seems like a great way to pick the best tool for the job and learn **a lot** along the way.

## Choosing where to start 

Since there are so many concurrency models available through this gem, I thought a good place to start would be to pick a model and write some code. The first option [in the list](https://github.com/ruby-concurrency/concurrent-ruby#general-purpose-concurrency-abstractions), [`Async`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html) happens to also be inspired by Erlang's [`gen_server`](https://elixir-lang.org/getting-started/mix-otp/genserver.html), so I thought that would be the perfect place to start. 

In Elixir/Erlang, a `gen_server` would run in a new Erlang process. This allows it to run independently of the caller's process. `concurrent-ruby` provides similar functionality with Ruby's [`Thread`](https://ruby-doc.org/core-2.7.0/Thread.html). 

When `include`d in a class, `Async` adds two new methods to the class that can be used as proxies for method calls. When used, these codes will proxy the code in the method to be run in a separate thread. These new methods are (1) `async` which will immediately return to the caller while continuing the work in the new thread and (2) `await` which will also do the work in the other thread, but, when called, will block (or (a)wait in) the main thread until the method called finishes and returns. _Note: if you (and your team) are familiar with the Erlang terms, you can also use `cast` (`async`) and `call` (`await`)._

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

Starting out in `irb` I was able to make the following calls.

#### Async Version

```ruby
irb(main):009:0> Hello.new.async.hello("world")
=> #<Concurrent::IVar:0x00007fe09b08b230 @__Lock__=#<Thread::Mutex:0x00007fe09b08b1b8>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b190>, @event=#<Concurrent::Event:0x00007fe09b08b118 @__Lock__=#<Thread::Mutex:0x00007fe09b08b0a0>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b078>, @set=false, @iteration=0>, @reason=nil, @value=nil, @observers=#<Concurrent::Collection::CopyOnWriteObserverSet:0x00007fe09b08b028 @__Lock__=#<Thread::Mutex:0x00007fe09b08afd8>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08afb0>, @observers={}>, @dup_on_deref=nil, @freeze_on_deref=nil, @copy_on_deref=nil, @do_nothing_on_deref=true, @state=:pending>
```

#### Await Version
irb(main):010:0> puts Hello.new.async.hello("world")
#<Concurrent::IVar:0x00007fe0ac2762f0>
```

Simply returning a string is fast enough that I didn't notice a speed difference between the two calls, but I do notice a difference in what is returned. Proxying through `async` and `await` result in method calls returning a [`Concurrent::IVar`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/IVar.html) instead of the original method's "raw" response. From the docs, an `IVar`  is described as:

> An `IVar` is like a future that you can assign. As a future is a value that is being computed that you can wait on, an `IVar` is a value that is waiting to be assigned, that you can wait on. `IVars` are single assignment and deterministic.

My time working with JavaScript has me assume this is similar enough to a [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) that I can continue to focus my learning on the `Async` module and come back to `IVar`s at a later time.

## Is anything happening? 

At this point, things seem to be working since I am getting back `IVar`s, but it doesn't _seem_ like the methods are performing any different. Often times, you would move work into another thread if it's slow and gets in the way of your main thread. To replicate this in my testing, I decided to add a [`sleep`](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-sleep). I also figured out a way to avoid dealing with `IVars` during my exploration; I can focus on printing output instead of worrying about return values. This gives me a visual indication my methods are being called without having to focus on the actual response. Now, my test class looks something like:

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

For my call to `await` this `sleep` is blocking, This means that when I call the method, it behaves similarly to calling it locally, even though it's running in a separate thread. This means that I have to wait for the `sleep` to finish before I can do anything:

<img src='./hello-await-with-sleep.gif' loading="lazy" />

However, with `async`, the method returns right away and the thread continues to run on its own. This means I can continue to interact with the main `irb` thread while waiting for my result to print:

<img src='./hello-async-with-sleep.gif' loading="lazy" />

## Conclusion

This is not even scratching the surface of the `Async` module, let alone the `concurrent-ruby`. However, I now have some code that behaves obviously different than running the methods on their own. This is a great starting point for digging into this module even more.


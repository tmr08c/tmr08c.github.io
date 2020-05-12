---
title: 'Concurrent Ruby - Hello, Async'
date: '2020-04-19T00:36:13.265Z'
categories: ['ruby', 'concurrency']
---

Over the last few years, I have been fiddling on-and-off with [Elixir](https://elixir-lang.org/) and have **loved** it. One of the reasons there is so much hype around Elixir is the [out-of-the-box support for writing highly concurrent code](https://elixir-lang.org/getting-started/processes.html). The concurrency model is a native part of the language that comes from the [Erlang](https://www.erlang.org/) VM which Elixir runs on. 

Recently, I have found myself working in the Ruby ecosystem. After so much focus on writing concurrent code, I wanted to see if I could continue to gain some multi-core benefits when I am working in Ruby. While I don't expect to see an Erlang level of concurrency support, I didn't think I should give up on concurrency altogether!

One option I found is the [`concurrent-ruby` gem](https:/github.com/ruby-concurrency/concurrent-ruby). From the gem's description:

> Modern concurrency tools including agents, futures, promises, thread pools, supervisors, and more. Inspired by Erlang, Clojure, Scala, Go, Java, JavaScript, and classic concurrency patterns.

Having multiple concurrency options available seems like a great way to pick the best tool for the job and learn **a lot** along the way.

## Choosing where to start 

Since there are so many concurrency models available through this gem, I thought a good place to start would be to simply pick a model and write some code. The first option [listed in the README](https://github.com/ruby-concurrency/concurrent-ruby#general-purpose-concurrency-abstractions) is their [`Async`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html)  module. This concurrency model also happens to be inspired by Erlang's (and therefore Elixir's) [`gen_server`](https://elixir-lang.org/getting-started/mix-otp/genserver.html), so it was the perfect place to start. 

In Elixir/Erlang, a `gen_server` would run in a new Erlang process (_insert disclaimer about Erlang's processes not being Operating System processes here_). This allows it to run independently of the caller's process. `concurrent-ruby` provides similar functionality with Ruby's [`Thread`](https://ruby-doc.org/core-2.7.0/Thread.html). 

When `include`d in a class, `Async` adds two new methods to the class that can be used as proxies. When used, these method will "wrap" the method you are calling. By wrapping the method, the proxies will handle the setup for running in a separate thread. These new methods are (1) [`async`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html#async-instance_method) which will immediately return to the caller while continuing the work in the new thread and (2) [`await`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html#await-instance_method) which will also do the work in the other thread, but, when called, will block (or (a)wait in) the main thread until the method called finishes and returns. If you (and your team) are familiar with the Erlang terms, you can also use `cast` (`async`) and `call` (`await`).

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

#### Await Version

```ruby
> puts Hello.new.async.hello("world")
 #<Concurrent::IVar:0x00007f9fdb909048 @__Lock__=#<Thread::Mutex:0x00007f9fdb908dc8>, @__Condition__=#<Thread::ConditionVariable:0x00007f9fdb908d50>, @event=#<Concurrent::Event:0x00007f9fdb908c10 @__Lock__=#<Thread::Mutex:0x00007f9fdb908b70>, @__Condition__=#<Thread::ConditionVariable:0x00007f9fdb908b48>, @set=true, @iteration=0>, @reason=nil, @value="Hello, world!", @observers=#<Concurrent::Collection::CopyOnWriteObserverSet:0x00007f9fdb913bd8 @__Lock__=#<Thread::Mutex:0x00007f9fdb913ae8>, @__Condition__=#<Thread::ConditionVariable:0x00007f9fdb913a70>, @observers={}>, @dup_on_deref=nil, @freeze_on_deref=nil, @copy_on_deref=nil, @do_nothing_on_deref=true, @state=:fulfilled>
```

#### Async Version

```ruby
> Hello.new.async.hello("world")
=> #<Concurrent::IVar:0x00007fe09b08b230 @__Lock__=#<Thread::Mutex:0x00007fe09b08b1b8>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b190>, @event=#<Concurrent::Event:0x00007fe09b08b118 @__Lock__=#<Thread::Mutex:0x00007fe09b08b0a0>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b078>, @set=false, @iteration=0>, @reason=nil, @value=nil, @observers=#<Concurrent::Collection::CopyOnWriteObserverSet:0x00007fe09b08b028 @__Lock__=#<Thread::Mutex:0x00007fe09b08afd8>, @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08afb0>, @observers={}>, @dup_on_deref=nil, @freeze_on_deref=nil, @copy_on_deref=nil, @do_nothing_on_deref=true, @state=:pending>
```

### Return Types

Simply returning a string is fast enough that I didn't notice a speed difference between the two calls, but I do notice a difference in what is returned. Proxying through `async` and `await` result in method calls returning a [`Concurrent::IVar`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/IVar.html) instead of the original method's "raw" response. From the docs, an `IVar`  is described as:

> An `IVar` is like a future that you can assign. As a future is a value that is being computed that you can wait on, an `IVar` is a value that is waiting to be assigned, that you can wait on. `IVars` are single assignment and deterministic.

Without digging too much into `IVar`s at this point, a few things stick out. 

In the `await` version, the `IVar` has a `@state` of `:fulfilled`.  We also have a `@value` of "Hello, world!" (the method's return value). Intuitively, this makes sense - with `await` we wait for the method to finish before continuing. By finishing the method call the work has been "fulfilled" and we know what our return value is. 

Contrast this with the result of the `async` version. The `async` call returns an `IVar` with a `@state` of `:pending` and a `@value` of `nil`. The `async` version returns without the method having necessarily been run yet, so we are still waiting for our results. 

My time working with JavaScript has me assume this is similar enough to a [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) that I can continue to focus my learning on the `Async` module and come back to `IVar`s at a later time. 

## Is anything happening? 

At this point, things seem to be working. I am getting back `IVar`s instead of the string the `hello` method returns, so the `Async` module is doing _something_. However, the `await` version runs so fast that it doesn't _seem_ like the methods are performing any differently. 

Often times, you would move work into another thread if it's slow and gets in the way of your main thread. To replicate this in my testing, I decided to fake working hard by adding a call to [`sleep`](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-sleep). I also decided to **print** the "hello" string instead of returning it. This allows me to avoid ~~dealing with~~ thinking about `IVars` during my initial exploration. Printing gives me a visual indication my methods are being called without having to focus on the actual response. 

Now, my test class looks something like:

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

After reloading this class into `irb`, I now have:

* A visual indication when the method has run from the `puts` statement.
* Methods that take longer to run as a result of the call to `sleep`. 

#### Await Version

When proxying through `await`, the call to `sleep` is blocking. This means that when I call the method, it behaves similarly to calling it locally, even though it's running in a separate thread. As a result, I have to wait for the `sleep` to finish before I can do anything else in the main thread (the `irb` session in this case):

<img src='./hello-await-with-sleep.gif' loading="lazy" />

#### Async Version

However, with `async`, the method returns right away and the main thread continues to run on its own. This means I can continue to interact with the main `irb` thread while waiting for my result to print:

<img src='./hello-async-with-sleep.gif' loading="lazy" />

## Conclusion

While we haven't done much,  we now have a class that provides us with `async` and `await` proxy methods.  We were also able to make the difference in behavior between these two proxy methods more obvious through the use of `puts` and `sleep`. 

This is not even scratching the surface of the `Async` module, let alone the entrie `concurrent-ruby` gem. However, this is a great starting point for digging into this module even more.


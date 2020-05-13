---
title: 'Concurrent Ruby - Hello, Async'
date: '2020-04-19T00:36:13.265Z'
categories: ['ruby', 'concurrency']
---

Over the last few years, I have been fiddling on-and-off with [Elixir](https://elixir-lang.org/) and have **loved** it. One of the reasons there is so much hype around Elixir is the out-of-the-box support for writing highly concurrent code. The concurrency model is a native part of the language and comes from the [Erlang](https://www.erlang.org/) VM which Elixir runs on. 

Despite working primarily in Ruby, I don't want to give up on the benefits of concurrency. While I don't expect to see an Erlang level of concurrency support, I believe Ruby has viable options.

One option I found is the [`concurrent-ruby` gem](https:/github.com/ruby-concurrency/concurrent-ruby). From the gem's description:

> Modern concurrency tools including agents, futures, promises, thread pools, supervisors, and more. Inspired by Erlang, Clojure, Scala, Go, Java, JavaScript, and classic concurrency patterns.

Having multiple concurrency paradigms provides the opportunity to pick-and-choose based on needs. It also provides the opportunity to learn about the various methods of concurrency all in one place.

## Choosing where to start 

I thought a good place to start would be to simply pick a concurrency model and write some code. The first option [listed in the README](https://github.com/ruby-concurrency/concurrent-ruby#general-purpose-concurrency-abstractions) is their [`Async`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html)  module. This concurrency model also happens to be inspired by Erlang's (and therefore Elixir's) [`gen_server`](https://elixir-lang.org/getting-started/mix-otp/genserver.html), so it was the perfect place to start. 

In Elixir/Erlang, a `gen_server` would run in a new Erlang process (_insert disclaimer about Erlang's processes not being Operating System processes here_). This allows it to run independently of the caller's process. `concurrent-ruby` provides similar functionality with Ruby's [`Thread`](https://ruby-doc.org/core-2.7.0/Thread.html). 

When `include`d in a class, `Async` adds two new methods to the class that can be used as proxies. When used, these methods will "wrap" the method you are calling. By wrapping the method, the proxies will handle the setup for running in a separate thread. These new methods are (1) [`async`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html#async-instance_method) which will immediately return to the caller while continuing the work in the new thread and (2) [`await`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html#await-instance_method) which will also do the work in the other thread, but, when called, will block (or (a)wait) the main thread until the method finishes and returns. 

If you (and your team) are familiar with the Erlang terms, you can use `cast` (`async`) and `call` (`await`) instead.

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
> puts Hello.new.await.hello("world")
=> #<Concurrent::IVar:0x00007fb62e8d5ec8
 @__Condition__=#<Thread::ConditionVariable:0x00007fb62e8d5e28>,
 @__Lock__=#<Thread::Mutex:0x00007fb62e8d5e50>,
 @copy_on_deref=nil,
 @do_nothing_on_deref=true,
 @dup_on_deref=nil,
 @event=
  #<Concurrent::Event:0x00007fb62e8d5db0
   @__Condition__=#<Thread::ConditionVariable:0x00007fb62e8d58d8>,
   @__Lock__=#<Thread::Mutex:0x00007fb62e8d5d38>,
   @iteration=0,
   @set=true>,
 @freeze_on_deref=nil,
 @observers=
  #<Concurrent::Collection::CopyOnWriteObserverSet:0x00007fb62e8d5888
   @__Condition__=#<Thread::ConditionVariable:0x00007fb62e8d57c0>,
   @__Lock__=#<Thread::Mutex:0x00007fb62e8d5838>,
   @observers={}>,
 @reason=nil,
 @state=:fulfilled,
 @value="Hello, world!">
 ```

#### Async Version

```ruby
> Hello.new.async.hello("world")
=> #<Concurrent::IVar:0x00007fe09b08b230 
@__Lock__=#<Thread::Mutex:0x00007fe09b08b1b8>, 
@__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b190>, 
@event=
 #<Concurrent::Event:0x00007fe09b08b118 
  @__Lock__=#<Thread::Mutex:0x00007fe09b08b0a0>, 
  @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08b078>, 
  @set=false, 
  @iteration=0>, 
@reason=nil, 
@value=nil, 
@observers=
 #<Concurrent::Collection::CopyOnWriteObserverSet:0x00007fe09b08b028 
   @__Lock__=#<Thread::Mutex:0x00007fe09b08afd8>, 
   @__Condition__=#<Thread::ConditionVariable:0x00007fe09b08afb0>, 
   @observers={}>, 
@dup_on_deref=nil, 
@freeze_on_deref=nil, 
@copy_on_deref=nil, 
@do_nothing_on_deref=true, 
@state=:pending>
```

### Return Types

Simply returning a string is fast enough that I didn't notice a speed difference between the two calls. I did, however, notice a difference in what is being returned. Proxying through `async` and `await` result in method calls returning a [`Concurrent::IVar`](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/IVar.html) instead of the original method's "raw" response. 

From the docs:

> An `IVar` is like a future that you can assign. As a future is a value that is being computed that you can wait on, an `IVar` is a value that is waiting to be assigned, that you can wait on. `IVars` are single assignment and deterministic.

Without digging too much into `IVar`s at this point, a few things stick out. 

In the `await` version, the `IVar` has a `@state` of `:fulfilled`.  We also have a `@value` of "Hello, world!" (the method's return value). Intuitively, this makes sense - with `await` we wait for the method to finish before continuing. By finishing the method call, the work has been "fulfilled" and we know what our return value is. 

Contrast this with the result of the `async` version. The `async` call returns an `IVar` with a `@state` of `:pending` and a `@value` of `nil`. The `async` version returns without waiting for the method to run. This means we are still waiting for our results. 

For now, I am going to assume this is similar enough to JavaScript's [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) that I can continue to focus on the `Async` module and come back to `IVar`s at a later time. 

## Is anything happening? 

At this point, things seem to be working. I am getting back `IVar`s instead of the string the `hello` method returns, so the `Async` module is doing _something_. However, the `await` version runs so fast that it doesn't _seem_ like the methods are performing any differently. 

Often times, you would move work into another thread if it's slow and gets in the way of your main thread. To replicate this in my testing, I decided to fake working hard by [`sleep`](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-sleep)ing. I also decided to print the "hello" string instead of returning it. This allows me to avoid ~~dealing with~~ thinking about `IVars` during my initial exploration. Printing gives me a visual indication the methods are running without having to worry about what is being returned.

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

* A visual indication from the `puts` statement of when the method has run.
* Methods that take longer to run as a result of the call to `sleep`. 

#### Await Version

When proxying through `await`, the call to `sleep` in `Hello#hello` is blocking. This means that, even though it's running in a separate thread, I have to wait for the `sleep` to finish before the main thread (the `irb` session in this case) will respond:

<img src='./hello-await-with-sleep.gif' />

#### Async Version

With `async`, the method returns right away and the main thread continues to run on its own. This means I can continue to interact with the main `irb` thread while waiting for my result to print:

<img src='./hello-async-with-sleep.gif' />

## Conclusion

While we haven't done much,  we now have a class that provides us with `async` and `await` proxy methods.  We were also able to make the difference in behavior between these two proxy methods more obvious through the use of `puts` and `sleep`. 

This is not even scratching the surface of the `Async` module, let alone the entire `concurrent-ruby` gem. What this does provide, however, is a great jumping-off point to explore the `Async` module further in the future.


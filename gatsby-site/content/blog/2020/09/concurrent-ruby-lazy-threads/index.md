---
title: 'Concurrent Ruby - Lazy Threads'
date: '2020-09-10T06:26:13.265Z'
categories: ['ruby', 'concurrency']
---

In this post, we try to understand how the [`concurrent-ruby`](https://github.com/ruby-concurrency/concurrent-ruby) gem leverages `Thread`s within its `Async` module.

In a [previous post](/2020/05/concurrent-ruby-hello-async/), we began the process of learning about the `concurrent-ruby` gem. In that post, we started with the "hello, world" example provided in the `Async` module's [documentation](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html) and made a few small tweaks to make the effects of `async` versus `await` obvious. We will now build on those foundations as we dive further into the `Async` module.

## Our Test Setup

Before we get into how `Thread`s are used in the `Async` module, let's take a look at the code we will be using for testing.

Here is the file we will be working with:

```ruby
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'concurrent-ruby'
end

require 'concurrent'

class HelloAsync
  include Concurrent::Async

  def hello_method
    sleep(3)
    puts "Hello! My object id is '#{object_id}' " \
         "and I'm running in thread " \
         "'#{Thread.current.object_id}'."
  end
end

hello_instance = HelloAsync.new

print '> '
while (input = gets)
  case input
  when /^[qQxX]/
    puts 'Quitting...'
    exit(0)
  when /^l(ist)?/
    puts "Currently have #{Thread.list.count} threads."
  when /^async/
    hello_instance.async.hello_method
  when /^await/
    hello_instance.await.hello_method
  when /^new-async/
    HelloAsync.new.async.hello_method
  when /^new-await/
    HelloAsync.new.await.hello_method
  else puts "Received unknown input: #{input}"
  end

  print '> '
end
```

If this makes sense, feel free to [skip ahead](#baseline) to the next section. Otherwise, read on for a breakdown of the code.

### Dependencies

While we only have one gem we need for our script (`concurrent-ruby`, the gem we are testing), we also rely on [Bundler](https://bundler.io/) for fetching this gem.

```ruby
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'concurrent-ruby'
end

require 'concurrent'
```

With [`bundler/inline`](https://bundler.io/guides/bundler_in_a_single_file_ruby_script.html), we define our `gemfile` in a block within the file. When running the script, if the gem isn't installed, Bundler will download it before proceeding.

### `HelloAsync` Class

This class is similar to what we created in the [previous post](/2020/05/concurrent-ruby-hello-async/). For more details on setting up a class to work with the `Concurrent::Async` module, please check out that post.

```ruby
class HelloAsync
  include Concurrent::Async

  def hello_method
    sleep(3)
    puts "Hello! My object id is '#{object_id}' " \
         "and I'm running in thread " \
         "'#{Thread.current.object_id}'."
  end
end
```

There are a few changes from our original implementation:

* The class has been renamed to `HelloAsync` and the method to `hello_method` to more easily differentiate between the class and method when writing about them.
* The `puts` statement has been updated to add some additional information for our experimentation, including:
  * The `object_id` for the current instance of the class. Since our REPL has an option for creating new objects (more on this below), this makes it possible to differentiate output between new and existing objects.
  * The `object_id` of the [`Thread` that the code is running in](https://ruby-doc.org/core-2.5.0/Thread.html#method-c-current). This helps us to identify whether we are in a new or existing `Thread`.

### Command Options

This script creates a basic [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) that can handle a small set of commands:

```ruby
while (input = gets)
  case input
  when /^[qQxX]/
    puts 'Quitting...'
    exit(0)
  when /^l(ist)?/
    puts "Currently have #{Thread.list.count} threads."
  when /^async/
    hello_instance.async.hello_method
  when /^await/
    hello_instance.await.hello_method
  when /^new-async/
    HelloAsync.new.async.hello_method
  when /^new-await/
    HelloAsync.new.await.hello_method
  else puts "Received unknown input: #{input}"
  end
end
```

These options enable tracking the program's thread count while using various combinations of the `async` and `await` proxy methods. Let's cover them in more detail:

|Command|Description|
|-|-|
|<kbd>q</kbd>, <kbd>x</kbd>| Quit. Break out of the REPL and stop the script.|
|<kbd>l</kbd>, <kbd>list</kbd>| Print the number of `Thread`s the Ruby process knows about. Uses [`Thread.list`](https://ruby-doc.org/core-2.5.0/Thread.html#method-c-list).|
|<kbd>async</kbd>| Run the `hello_method` method through the `async` proxy on an **existing** instance of the `HelloAsync` class.|
|<kbd>new-async</kbd>| Instantiates a **new** instance of the `HelloAsync` class and runs the `hello_method` method through the `async` proxy.|
|<kbd>await</kbd>| Run the `hello_method` method through the `await` proxy on an **existing** instance of the `HelloAsync` class.|
|<kbd>new-await</kbd>| Instantiates a **new** instance of the `HelloAsync` class and runs the `hello_method` method through the `await` proxy.|

### Full File

Pulling it all together again, we have the following:

```ruby
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'concurrent-ruby'
end

require 'concurrent'

class HelloAsync
  include Concurrent::Async

  def hello_method
    sleep(3)
    puts "Hello! My object id is '#{object_id}' " \
         "and I'm running in thread " \
         "'#{Thread.current.object_id}'."
  end
end

hello_instance = HelloAsync.new

print '> '
while (input = gets)
  case input
  when /^[qQxX]/
    puts 'Quitting...'
    exit(0)
  when /^l(ist)?/
    puts "Currently have #{Thread.list.count} threads."
  when /^async/
    hello_instance.async.hello_method
  when /^await/
    hello_instance.await.hello_method
  when /^new-async/
    HelloAsync.new.async.hello_method
  when /^new-await/
    HelloAsync.new.await.hello_method
  else puts "Received unknown input: #{input}"
  end

  print '> '
end
```

If you are interested in trying this out for yourself, it's also available [on GitHub](https://github.com/tmr08c/trying-concurrent-ruby/blob/master/01-hello-async.rb).

## Baseline

When starting the REPL and printing out the thread count I see the following:

```markup
> list
Currently have 2 threads.
```

I didn't _expect_ two threads, but maybe Ruby leverages threads more than I thought. Let's compare this with an `irb` session:

```ruby
irb> Thread.list
=> [#<Thread:0x00007fbef585ffa0 run>]
```

Hmm, okay, it looks like we only have one thread. In the `irb` session I used `Thread.list` and dropped the `count`. I am going to temporarily drop the `count` in my script as well.

```markup
> list
Currently have [
  #<Thread:0x00007fdacc064010 run>,
  #<Thread:0x00007fdacc100438@.../concurrent-ruby/concurrent/atomic/ruby_thread_local_var.rb:38 sleep_forever>
] threads.
```

So we have the same "run" thread, but we also have a thread that looks like it's related to the `concurrent-ruby` gem.

### concurrent-ruby Initialize Thread

This other thread is created during the `require` process of the
`concurrent-ruby` gem (it looks like there is
[discussion](https://github.com/ruby-concurrency/concurrent-ruby/issues/868) of
whether that is the right time to do this) as a part managing
[`ThreadLocalVar`](https://ruby-concurrency.github.io/concurrent-ruby/1.1.5/Concurrent/ThreadLocalVar.html)s.
This means that the additional thread is created _prior_ to creating a new
instance of our `HelloAsync` class - it's created as soon as we `require
'concurrent'`.

We can reproduce this in `irb` as well:

```ruby
irb> Thread.list
=> [#<Thread:0x00007f912a85ffa8 run>]

irb> require 'concurrent'
=> true

irb> Thread.list
=> [#<Thread:0x00007f912a85ffa8 run>,
    #<Thread:0x00007f91299453b0@.../concurrent-ruby/concurrent/atomic/ruby_thread_local_var.rb:38 sleep_forever>
]
```

The implementation tracks and manages `ThreadLocalVar`s in a [long-running thread](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/atomic/ruby_thread_local_var.rb#L38-L39). We haven't yet begun digging into the [thread-safe objects the library provides](https://github.com/ruby-concurrency/concurrent-ruby#thread-safe-value-objects-structures-and-collections) yet, so we shouldn't need to worry too much about this additional thread.

The important things to note are:

1. The baselines thread count is two.
1. The baseline does **not** include any threads created by the `HelloAsync` class.

### Multiple Rubies Support

You may note that the file referenced in the thread is `ruby_thread_local_var.rb` and not just `thread_local_var.rb`. `ThreadLocalVar` has [implementations for Ruby and JRuby](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/atomic/thread_local_var.rb#L60-L65). Since I am using MRI, I am seeing `RubyThreadLocalVar` and not `JavaThreadLocalVar`.

I mention this because there may be other instances where I reference the gem's MRI implementation, but you may see something different.

## Our first (or third) thread

In our [previous post](/2020/05/concurrent-ruby-hello-async/) we learned that the `await` method would create a new thread, run the method in the new thread, and return to the main thread; blocking our main thread the whole time. Does the thread stick around when it is done running the method?

```markup
> await
Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.
```

It seems it does.

Does this mean that every time we use one of our proxy methods we are going to have extraneous threads that stick around?

```markup
> await
Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.

> await
Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.
```

When running the `await` command multiple times, we are seeing the same object and thread IDs. The object ID makes sense since in our [REPL](#command-options) we had the `await` command use the same object. However, the same thread ID wasn't something we intentionally set up.

This means we are **reusing our thread**. Rather than having threads created and left around, the library is reusing them for subsequent calls. This is great, as it helps save on the cost of starting up and maintaining a new thread.

## Fancy another

So far, we've learned that `concurrent-ruby` will reuse our thread when calling `await` on the same instance of a class. What happens if we instantiate a new instance of our `HelloAsync` class? This is where the [`new-await` command](#command-options) in our REPL comes into play.

```markup
> await
Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.

> new-await
Hello! My object id is '70161462088680' \
and I'm running in thread '70161462091140'.
```

On closer inspection, we _are_ seeing a new object ID, but the same thread ID:

```{diff}
- Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

+ Hello! My object id is '70161462088680' \
and I'm running in thread '70161462091140'.
```

This indicates that `concurrent-ruby` will reuse threads, even across new instances of our `Async`-inheriting classes.

## What are you (a)waiting for

At this point, all of our tests have only used the `await` proxy method. Since this method will block our main thread until it's complete, we aren't sending multiple requests to multiple objects at a time. This does seem like it would make it easier to reuse the same thread. Do we see the same behavior with `async`?

Let's start with our `async` action. This will run the `hello_method` method through the `async` proxy on our existing instance of `HelloAsync`.

``` markup
> async
Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.
```

Since we are using our existing instance, it makes sense to see our same object ID. After what we've learned so far about thread reuse, it's not a complete surprise to see the same thread ID again either (despite a different proxy method being used).

As we mentioned above, since `async` doesn't block our main thread, we can call it multiple times. If we can get multiple calls to `async` queued up, should things be running concurrently through multiple threads?

```markup
> async

> list
Currently have 3 threads.

> async

> list
Currently have 3 threads.

> async

Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.

> async

> list
Currently have 3 threads.

Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.

Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.

> list
Currently have 3 threads.

Hello! My object id is '70161458709520' \
and I'm running in thread '70161462091140'.
```

Above, we have multiple calls to `async` and `list`. The goal was to see if calling `async` multiple times before the method is completed (and we see our print statement) could increase our thread count. We seem to still be using our same object ID and thread ID, and never go above three threads. What's the deal? How are we going to do things concurrently if we don't spawn more threads?

### Actors and Mailboxes

Because the `Async` module is modeling itself off of [`gen_server`](https://erlang.org/doc/man/gen_server.html), it uses the [message-passing semantics](https://en.wikipedia.org/wiki/Actor_model#Message-passing_semantics) of the Actor model.

[This StackOverflow answer](https://stackoverflow.com/a/10816216/2475008) from erlang co-creator Robert Virding explains what that means:

> All messages are placed in a process' message queue and processes handle their message one-by-one. If a message arrives while a process is busy then it is placed in the message queue.

Staying true to their inspiration, `concurrent-ruby` follows a similar idea of queuing up the method calls an object receives. The object will then process (i.e., run) the methods one at a time. [Here](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/async.rb#L323-L334) is the relevant code in the gem:

```ruby
# This is in the `AsyncDelegator`
# class defined within the `Async` module
def method_missing(method, *args, &block)
  super unless @delegate.respond_to?(method)
  Async::validate_argc(@delegate, method, *args)

  ivar = Concurrent::IVar.new
  synchronize do
    @queue.push [ivar, method, args, block]
    @executor.post { perform } if @queue.length == 1
  end

  ivar
end
```

Let's try to break this down. Before we get into this `method_missing` we have to learn a little bit more about the `Async` module.

The `Async` module defines `await` and `async` methods. Here is the `async` method:

```ruby
def async
  @__async_delegator__
end
```

This returns an instance variable, `@__async_delegator__` which is created as a part of the initialization process. The `AsyncDelegator` is initialized by passing in `self`. `self` is the instance of the object that is _calling_ the proxy method. In our case it is `hello_instance`, our `HelloAsync` object.

```ruby{4}
def init_synchronization
  return self if defined?(@__async_initialized__) && @__async_initialized__
  @__async_initialized__ = true
  @__async_delegator__ = AsyncDelegator.new(self)
  @__await_delegator__ = AwaitDelegator.new(@__async_delegator__)
  self
end
```

The `AsyncDelegator` class is defined within the `Async` module. This `AsyncDelegator` class includes the `method_missing` method we saw above.

Here's a reminder of what is happening when we type `async` into our REPL:

```ruby
hello_instance = HelloAsync.new

# ...
when /^async/ then hello_instance.async.hello_method
```

We call `async` on `hello_instance`. We now know this is giving us an `AsyncDelegator` that has a reference to our `hello_instance` object. We then call the method `hello_method`. However, we aren't calling it on `hello_instance` (our instance of the `HelloAsync` class) but instead on our instance of `AsyncDelegator` (`@__async_delegator__`). While we haven't looked at the [whole `AsyncDelegator` class](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/async.rb#L301-L364), you can probably trust me that it doesn't define a `hello_method` method.

_This_ is where the `method_missing` from above comes into play. If you are unfamiliar with [`method_missing`](https://apidock.com/ruby/BasicObject/method_missing), it is invoked when a method is called on an object and that object doesn't have a definition for that method. It is a powerful tool for metaprogramming and is enabling our `AsyncDelegator` to accept method calls without having to explicitly define them.

```ruby
def method_missing(method, *args, &block)
```

When we call `hello_instance.async.hello_method`, our `AsyncDelegator` (which doesn't define a `hello_method` method) will invoke `method_missing` and pass `hello_method` as the `method` argument. It will also pass any other arguments and a block if one is included.

After invoking `method_missing`, the library will do some validations. First, it will check if the "delegate" object (`hello_instance` in our case) defines the method we are calling. It will then check if the right number of arguments were passed in.

```ruby
super unless @delegate.respond_to?(method)
Async::validate_argc(@delegate, method, *args)
```

After that, we create our `IVar`. In our [previous post](/2020/05/concurrent-ruby-hello-async/) we took a surface-level look at `IVar`s and decided to not dig into them yet. We will do the same thing here, but note that the `IVar` is created, some stuff happens (which we will cover next), and then it is returned. This lines up with what we saw in our initial experimentation - we [found](/2020/05/concurrent-ruby-hello-async/#return-types) that when using our proxy methods the return value would be an `IVar` instead of what the original method was returning.

```ruby
  # ...stuff from above

  ivar = Concurrent::IVar.new

  # Hiding the good stuff for now

  ivar
end
```

Next, comes the main functionality.

```ruby
synchronize do
  @queue.push [ivar, method, args, block]
  @executor.post { perform } if @queue.length == 1
end
```

The implementation of `synchronize` will vary by which type and version of Ruby you are running, but is a mechanism for working with locks. It will check if the thread has access to the lock before `yield`ing and running whatever is in the block.

Once we have synchronized, we add an array of information to `@queue`.   `@queue` is an array that is created in our initialization process and is the equivalent of the "mailbox" concept from erlang.

We are adding the `ivar`, `method`, and the method's arguments and block to the queue. This is everything that is needed to run our method and put the results in an `IVar` to be consumed later. We will then `post` a block to our `@executor`.

Finding the `@executor` is another chain of branching inheritance. For my version of MRI Ruby, I eventually got to [`RubyExecutorService#post`](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/executor/ruby_executor_service.rb#L17-L25). The `ExecutorService` classes is the abstraction for running your code in new threads. These classes take in the arbitrary code to run and handle interacting with threads.

Along the chain to the `RubyExecutorService`, one of the parent classes was [`CachedThreadPool`](https://www.rubydoc.info/gems/concurrent-ruby/Concurrent/CachedThreadPool). **This class is key for many of the observations we have seen so far**. From the docs:

> A thread pool that dynamically grows and shrinks to fit the current workload. New threads are created as needed, existing threads are reused, and threads that remain idle for too long are killed and removed from the pool.

While this `CachedThreadPool` pool explains _some_ of what we've seen, it doesn't fully explain why we aren't seeing the thread pool grow dynamically when we proxy through `async` multiple times. For that, we will need to look at the `perform` method in `AsyncDelegator`. This method is what is passed into our `@executor` to be run in a separate thread

```ruby
# Async::AsyncDelegator
def perform
  loop do
    ivar, method, args, block = synchronize { @queue.first }
    break unless ivar # queue is empty

    begin
      ivar.set(@delegate.send(method, *args, &block))
    rescue => error
      ivar.fail(error)
    end

    synchronize do
      @queue.shift
      return if @queue.empty?
    end
  end
end
```

`perform` is passed into the `@executor` and, when invoked, will loop until the `@queue` is empty. Each iteration of the loop will run the method originally called and update the `ivar`.

So, why do we need the `if` on the following line?

```ruby
@executor.post { perform } if @queue.length == 1
```

If `@queue` isn't empty, the `executor` would still be in the `loop` in `perform`. When we mutate the `@queue` by calling the `push` method it is updated everywhere, so our `loop` will have access to the new element pushed onto the queue. This explains the need to `synchronize` - when mutating the state of `@queue` we need to lock first because we could be updating it in our main thread (when we add to it by calling the method through the proxy) or in our `@executor` thread (in the `perform` loop when we `shift` elements off of the queue).

This shows how the `gen_server`-style FIFO message queue is implemented in the `Async` module.

* An `AsyncDelegator` instance has an instance variable `@queue` that is an array and stores everything needed to run the method called.
* This array is passed in to the `perform` method, through a thread pool, and finally to an executor thread that will eventually run our code.
* In `perform` the thread will loop, executing the next method in the queue, until there is nothing left in the queue.

## I came here to see multiple threads

So far, we've seen the efficiency of `CachedThreadPool` and the message queue implementation style of `gen_stage` and how this has resulted in no additional threads spawned. What does it take to make a new thread?

### Can we spawn one, now

We've already seen `CachedThreadPool` pool in action when working with new instances of the `HelloAsync` class through our `new-await` command [above](#fancy-another). It may not be surprising to find that `new-async` (when called when no other commands are running) behaves similarly:

```markup
> new-async

> list
Currently have 3 threads.

Hello! My object id is '70161458746800' \
and I'm running in thread '70161462091140'.
```

### Okay, how about now

So, again, `CachedThreadPool` can reuse threads across instances of our `HelloAsync` class. But if we re-read the class's description, we know that threads are created "as needed."

> New threads are created as needed, existing threads are reused, (...)

So far, we haven't see the "as needed" case. What if we run _multiple_ `new-async` commands? Since it's `async` we should be able to request them multiple times view our REPL and since each request goes to a new instance, there is no waiting in the queue.

```markup
> new-async
> new-async
> new-async
> new-async
> new-async
> new-async
> new-async

> list
Currently have 9 threads.

Hello! My object id is '70161189799440' \
and I'm running in thread '70161462091140'.

Hello! My object id is '70161189798560' \
and I'm running in thread '70161458745000'.

Hello! My object id is '70161458781960' \\
and I'm running in thread '70161458781240'.

Hello! My object id is '70161458780740' \
and I'm running in thread '70161458780020'.

Hello! My object id is '70161458779520' \
and I'm running in thread '70161458778800'.

Hello! My object id is '70161458778300' \
and I'm running in thread '70161458777580'.

Hello! My object id is '70161458777080' \
and I'm running in thread '70161458776360'.

```

At last! We've found a way to spawn more threads.

## Conclusion

While we haven't _used_ the `Async` module in any meaningful way yet, these simple thread count checks have begun to lead us through implementation details of the `concurrent-ruby` gem. Hopefully, this exploration has provided us with a better understanding and appreciation of the gem.

I initially set out expecting to see a lot of thread creation. Instead, I learned that, thanks to `concurrent-ruby`, threads can be lazy (in the best way!). While the message processing aspects of `gen_stage` (and therefore the `Async` module) played a role in the lack of needing to spawn as many threads, it was the abstractions provided by `concurrent-ruby` that helped make things seamless. Since I imagine most people reach for a gem like `concurrent-ruby` for performance reasons, it's great to know that it has your back.

If you're interested in trying out different ways to create threads with the `Async` module, the code for the REPL is available [on
GitHub](https://github.com/tmr08c/trying-concurrent-ruby/blob/master/01-hello-async.rb).


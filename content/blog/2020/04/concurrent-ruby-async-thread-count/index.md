---
title: 'Concurrent Ruby - Lazy Threads'
date: '2020-04-25T00:36:13.265Z'
categories: ['ruby', 'concurrency']
---

In a [previous post](/2020/05/concurrent-ruby-hello-async/), I began my process of learning about the [`concurrent-ruby`](https://github.com/ruby-concurrency/concurrent-ruby) gem. In that post, I started with the "hello, world" example provided in the [documentation of the `Async` module](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Async.html). I made a small tweak on the example to `sleep` and `puts` the string to make the affects of `async` versus `await` more obvious. In this post, I try to understand the usage of `Thread`s within the `Async` module. 

## CLI

For my exploration into thread usage, I am able to continue my use of the example`Hello` class. I have updated the `puts` statement slightly to add additional details that we will cover later.  I also wrote up a small CLI around it. Thanks to [`bundler/inline`](https://bundler.io/guides/bundler_in_a_single_file_ruby_script.html) I was able to work within a single file during testing. The end result is the following:

```ruby
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'concurrent-ruby'
end

require 'concurrent'

class HelloAsync
  include Concurrent::Async

  def hello
    sleep(3)
    puts "Hello! My object id is '#{object_id}' and I'm running in thread '#{Thread.current.object_id}'."
  end
end

hello = HelloAsync.new

print '> '
while (input = gets)
  case input
  when /^[qQxX]/
    puts 'Quitting...'
    exit(0)
  when /^l(ist)?/ then puts "Currently have #{Thread.list.count} threads."
  when /^async/ then hello.async.hello
  when /^await/ then hello.await.hello
  when /^new-async/ then HelloAsync.new.async.hello
  when /^new-await/ then HelloAsync.new.await.hello
  else puts "Received unknown input: #{input}"
  end

  print '> '
end
```

If you are interested in trying this out for yourself, I have a copy [on GitHub](https://github.com/tmr08c/trying-concurrent-ruby/blob/master/01-hello-async.rb).

### `HelloAsync` Class

As mentioned previously, this class remained similar to what we created in a [previous post](/2020/05/concurrent-ruby-hello-async/). You may want to check out that post for some additional context. 


As mentioned above, the `puts` statement in `Hello#hello` has been updated to include additional information. The `puts` statement will now print the `object_id` for the current instance of the `Hello` class as well as the `Thread` that the code is being in. This will help us see the impact of creating new instances of the `Hello` class and understand how `concurrent-ruby` works with `Thread`s.

### Command Options

This script creates a super simple REPL that can handle a small set of commands.

|Command|Description|
|-|-|
|<kbd>q</kbd>, <kbd>x</kbd>| **Quit**. Break out of the REPL and stop the script|
|<kbd>l</kbd>, <kbd>list</kbd>| **List** the count of `Thread`s the Ruby process knows about. Uses [`Thread.list`](https://ruby-doc.org/core-2.5.0/Thread.html#method-c-list).|
|<kbd>async</kbd>| Run the `hello` method through the `async` proxy on an already created instance of the `HelloAsync` class.|
|<kbd>new-async</kbd>| Instantiated a new instance of the `HelloAsync` class and run the `hello` method through the `async` proxy  on it.|
|<kbd>await</kbd>| Run the `hello` method through the `await` proxy on an already created instance of the `HelloAwait` class.|
|<kbd>new-await</kbd>| Instantiated a new instance of the `HelloAsync` class and run the `hello` method through the `await` proxy  on it.|

These options enabled me to better understand how the `concurrent-ruby` library managed threads for the `Async` modules. From a single session I am able to track the thread count while testting the `async` und `await` proxies on existing and new instances of a class that includes the `Async` module. 

## Baseline

When strating the REPL and printing out the thread I see the following:

```bash
> list
Currently have 2 threads.
```

I didn't _expect_ two threads, but maybe Ruby leverages multiple threads more than I thought. Let's compare this when an `irb` session:

```ruby
irb(main)> Thread.list
=> [#<Thread:0x00007fbef585ffa0 run>]
```

Hm, okay, it looks like we have one "run" thread. In the `irb` session I used `Thread.list` and dropped the `count`. I am going to temporairily drop the `count` in my script as well. 

```bash
> list
Currently have [
  #<Thread:0x00007fdacc064010 run>,
  #<Thread:0x00007fdacc100438@/Users/troyrosenberg/.asdf/installs/ruby/2.6.5/lib/ruby/gems/2.6.0/gems/concurrent-ruby-1.1.6/lib/concurrent-ruby/concurrent/atomic/ruby_thread_local_var.rb:38 sleep_forever>] threads.
```

So we have the same "run" thread, but we also have a thread that looks like it's realted to the `concurrent-ruby` gem. This other thread is created during the `require` process of the `concurrent-ruby` gem (it looks like there is [discussion](https://github.com/ruby-concurrency/concurrent-ruby/issues/868) of whether that is the right time to do this) as a part managing [`ThreadLocalVar`](https://ruby-concurrency.github.io/concurrent-ruby/1.1.5/Concurrent/ThreadLocalVar.html)s. This means that the additonal thread is crated prior to creating a new instance of our `HelloAsync` class - it's created as soon as we `require 'concurrent-ruby'`.

You may note that the file referenced in the thread is "ruby_thread_local_var.rb" and not just "thread_local_var.rb". `ThreadLocalVar` has [implementations for Ruby and jRuby](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/atomic/thread_local_var.rb#L60-L65). Since I am using MRI, I am seeing `RubyThreadLocalVar` and not `JavaThreadLocalVar`. 

From [the docs](https://ruby-concurrency.github.io/concurrent-ruby/1.1.5/Concurrent/ThreadLocalVar.html):

> A ThreadLocalVar is a variable where the value is different for each thread. Each variable may have a default value, but when you modify the variable only the current thread will ever see that change.

The implementation tracks and manages `ThreadLocalVar`a in a [long-running thread](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/atomic/ruby_thread_local_var.rb#L38-L39). We haven't yet begun digging into the [thread-save objects the library provides](https://github.com/ruby-concurrency/concurrent-ruby#thread-safe-value-objects-structures-and-collections) yet, so we shouldn't need to worry too much about this additional thread. The important thing to note is that our baselines thread count is two and that does not include any threads we create with out `HelloAsync` class.


## Our first (or third) thread

In our [previous post](/2020/05/concurrent-ruby-hello-async/) we learned that the `await` method would create a new thread, run the method in the new thread, and return to the main thead; blocking our main thread the whole time. Does the thread stick around when the method is done? 

```bash
> await
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> l
Currently have 3 threads.
```

It seems it does. Does this mean that every time we use one of our proxy methods we are creating new threads? 

```
> await
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> l
Currently have 3 threads.
> await
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> l
Currently have 3 threads.
```

No, I did not accidentally paste something twice. When running the `await` command again a few seconds later, we are seeing the same object and thread IDs. The object ID  makes sense since in our [CLI](#cli) we had the `await` command use the same object. However, the same thread ID wasn't something we intentionally set up. This shows us that we are reusing our thread. This is great as it helps save on the cost of starting up a new thread.


- [ ] (maybe) Inclue some links or code examples showing where in the library thread re-use is happening 

## Fancy another? 

So far, we've learned that `concurrent-ruby` we re-use our a thread when calling `await` on the same instance of a class. What happens if we instantiate a new instance of our `HelloAsync` class? This is where our `new-await` options from our [CLI](#cli) comes into play.

```bash
> await
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> list
Currently have 3 threads.
> new-await
Hello! My object id is '70161462088680' and I'm running in thread '70161462091140'.
```

On closer inspectin, we _are_ seeing a new object ID, but the same thread:

```diff
- Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
+ Hello! My object id is '70161462088680' and I'm running in thread '70161462091140'.
```

Again, this indicates that `concurrent-ruby` will reuse threads, evenv across new instances of our `Async`-inheriting clases.

## What are you (a)waiting for!?

At this point, all of our tests have currently only used the `await` proxy method. Since this method will block our main thread until it's complete, we aren't sending multiple requests to multiple objects at a time. This does seems like it would make it easier to re-use threads. Do we see the same behavior with `async`?

Let's start our with our `async` action. This will run the `hello` method through the `async` proxy on our _existing_ instance of `HelloAsync`.

``` bash
> async
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
```

Since we are using our existing instance, it makes sense to see our same object ID. After what we've learned so far about thread re-use, it makes sense to see the same thead ID again as well.

As we mentioned above, since `async` doesn't block our main thread, we can call it multiple times. If we can get multiple calls to `async` queued up, should things be running concurrently and therefor in multiple threads?

```bash
> async
> list
Currently have 3 threads.
> async
> list
Currently have 3 threads.
> async
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> list
Currently have 3 threads.
> async
> list
Currently have 3 threads.
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> list
Currently have 3 threads.
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> list
Currently have 3 threads.
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
```

Above, we have multiple calls to `async` and `list`. The goal was to see if calling async multiple times before the method is completed (and we see our print statment) could increase our thread count. We seem to still be using our same object ID and thread ID and never go above three threads. What's the deal? How are we going to do things concurrently if we don't spawn more threads? 

### Actors and Mailboxes

Because the `Async` module is modeling itself off of [`gen_server`](https://erlang.org/doc/man/gen_server.html), it uses the [message-passing semantics](https://en.wikipedia.org/wiki/Actor_model#Message-passing_semantics) of the Actor model. This means when a method is called, rathern than running right away, the method is put into the "mailbox" to be processed by the object. Messages are processed one at a time in the order they are received. 

[This StackOverflow answer](https://stackoverflow.com/a/10746181/2475008) does a good job explaining it:

> The gen_server runs in a separate process from your client process so when
> you do a call/cast to it you are actually sending messages to server process.
> All messages are placed in a processes message queue and processes handle
> their message one-by-one. 

[Here](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/async.rb#L323-L334) is the relevant code in the gem:

```ruby
# This is in the `AsyncDelegator` class defined within the `Async` module
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

Let's try to break this down. Before we get into this `method_missing`, we have to learn a little bit about how things are set up.

The `Async` module defines `await` and `async` methods. Let's take a look at the `async` method:

```ruby
def async
  @__async_delegator__
end
```

This returns an instance variable, `@__async_delegator__`. This instance variable is set up as a part initialization and is a new `AsyncDelegator` (a class defined within this modle):

```ruby
def init_synchronization
  return self if defined?(@__async_initialized__) && @__async_initialized__
  @__async_initialized__ = true
* @__async_delegator__ = AsyncDelegator.new(self)
  @__await_delegator__ = AwaitDelegator.new(@__async_delegator__)
  self
end
```

The `AsyncDelegator` class is defined within the `Async` module. This brings us to our `method_missing` method above. Here's a reminder of what's happening when type `async` into our REPL:

```ruby
# HelloAsync has the Async module included
hello = HelloAsync.new

# ...
when /^async/ then hello.async.hello
```

We call `async` on `hello`. We now know this is giving us an `AsyncDelegator` that has reference to our `hello` object. We then call the method `hello`. However, we aren't calling it on our `hello` class, but instead on our instance of `AsyncDelegator` (`@__async_delegator__`). While we haven't looked at the [whole `AsyncDelegator` class](https://github.com/ruby-concurrency/concurrent-ruby/blob/082c05f136309fd7be56e7c1b07a4edcb93968f4/lib/concurrent-ruby/concurrent/async.rb#L301-L364), you can probably trust me that it doesn't define a `hello` method. _This_ is where the `method_missing` from above comes into play. If you are unfamiliar with [`method_missing`](https://apidock.com/ruby/BasicObject/method_missing) it is invoked when a method is called on an object, but that object doesn't have a definition for that method. It is a powerful tool for metaprgrogramming and is enabling our `AsyncDelegator` to accept method calls without having to explicitly define them. 

```ruby
def method_missing(method, *args, &block)
```

So when we call `hello.async.hello`, our `AsyncDelegator`, which doesn't define a `hello` method, will invoke `method_missing` and pass `hello` as the `method` argument. It will also pass any other aguments or block.


After invoking `method_missing`, the library will do some validations. First, it will check if the "delegate" object (`hello` in our case) defines the method we are calling. It will then check if the right number of arugments were passed in. 

```ruby
super unless @delegate.respond_to?(method)
Async::validate_argc(@delegate, method, *args)
```

After that we create our `IVar`. In our [previous post](/2020/05/concurrent-ruby-hello-async/) we took a surface-level look at `IVar`s and decided to not dig into them yet. We will do the same thing here, but note that the `ivar` is created some stuff happens (which we will cover next), and then returned; lining up with what we saw in our initial experimentation.  


```ruby
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

The implementation of `synchronize` will vary by which type and version of Ruby you are running, but is a meacanism for working with locks. It will check if the Thread has access to the lock before `yield`ing and running whatever is in the bock. It's not clear to me why we need a lock here. My guess is that since `AsyncDelegator` can run any arbitrary method, it's possible it could be accessing something like variable shared acroess threads. Since `concurrent-ruby` provides thread-safe variables, it needs to assume anytime a thread is running it could be attempting to work with one of these types of variables, and, therefore, needs to work with some for of locking mechanism to ensure it is safe to interact with these variables.

Once we have syncronized, we add an array of information to `@queue`. `@queue` is an arry that is created in our initialization process. We are adding the `ivar`, `method`, and the method's arguments and block to the queue. This is everything that iis needed to run our method and put the results in an `IVar` to be consumed later.



- [ ] Try to figure out `executor` and `perform` stuff
- [ ] Guess that queue variable will grow within executor but will lose reference when empty, so need to rebost when 0 --> 1

* same async class uses same thread
* running asyn multiple times still same thread (gen_stage and queues)
* new-async, new threads

```
› ruby 01-hello-async.rb 
> await
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> l
Currently have 3 threads.
> await
Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
> l
Currently have 3 threads.
> new-await
Hello! My object id is '70161462088680' and I'm running in thread '70161462091140'.
> new-await
Hello! My object id is '70161462087480' and I'm running in thread '70161462091140'.
> async
> Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
     
Received unknown input: 
> async
> l
Currently have 3 threads.
> async
> l
Currently have 3 threads.
> asHello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
ync
> l
Currently have 3 threads.
> async
> l
Currently have 3 threads.
> Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
l
Currently have 3 threads.
> Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
l
Currently have 3 threads.
> Hello! My object id is '70161458709520' and I'm running in thread '70161462091140'.
new-async
> l
Currently have 3 threads.
> new-async
> l
Currently have 4 threads.
> neHello! My object id is '70161458746800' and I'm running in thread '70161462091140'.
w-async
> l
Currently have 4 threads.
> Hello! My object id is '70161458745720' and I'm running in thread '70161458745000'.
new-async
> Hello! My object id is '70161458744080' and I'm running in thread '70161462091140'.
l
Currently have 4 threads.
> new-async
> l
Currently have 4 threads.
> newHello! My object id is '70161458742780' and I'm running in thread '70161458745000'.
-async
> l
Currently have 4 threads.
> Hello! My object id is '70161458741480' and I'm running in thread '70161462091140'.
new-async
> l
Currently have 4 threads.
> nHello! My object id is '70161189804980' and I'm running in thread '70161458745000'.
ew-async
> l
Currently have 4 threads.
> newHello! My object id is '70161189803680' and I'm running in thread '70161462091140'.
-async
> l
Currently have 4 threads.
> Hello! My object id is '70161189802380' and I'm running in thread '70161458745000'.
Hello! My object id is '70161189801080' and I'm running in thread '70161462091140'.

Received unknown input: 
> new-async
> new-async
> new-async
> new-async
> new-async
> new-async
> new-async
> l
Currently have 9 threads.
> Hello! My object id is '70161189799440' and I'm running in thread '70161462091140'.
Hello! My object id is '70161189798560' and I'm running in thread '70161458745000'.
Hello! My object id is '70161458781960' and I'm running in thread '70161458781240'.
Hello! My object id is '70161458780740' and I'm running in thread '70161458780020'.
Hello! My object id is '70161458779520' and I'm running in thread '70161458778800'.
Hello! My object id is '70161458778300' and I'm running in thread '70161458777580'.
Hello! My object id is '70161458777080' and I'm running in thread '70161458776360'.
```


* When are threads made
  * adding thread counter to CLI
  * trying with `.new`
  * `.new` and calling method (works), uses same thread for same variable because it's copying the actor model and queue 
      * to show this, maybe we should update `#hello` to have a print statement at the top like "I received this message"
  * can make new instances and call method everytime to make it work
  * lazy thread creation - eventually get to https://github.com/ruby-concurrency/concurrent-ruby/blob/7dc6eb04142f008ffa79a59c125669c6fcbb85a8/lib/concurrent-ruby/concurrent/executor/ruby_executor_service.rb#L17-L25
* conclusion
  * not sure how other parts work
  * next time - using async module (copy key/value store in elixir docs?)?

TODO

- [ ] (maybe) try calling a different method on the same class and see if things are the same
- [ ] (maybe) set up ling highlighting for prism (https://www.gatsbyjs.org/packages/gatsby-remark-prismjs/#line-highlighting)
- [ ] Fix style for table
- [x] Add style for `kbd` tag


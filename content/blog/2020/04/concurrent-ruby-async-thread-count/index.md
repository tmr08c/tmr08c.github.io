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

- [ ] Fix style for table
- [x] Add style for `kbd` tag

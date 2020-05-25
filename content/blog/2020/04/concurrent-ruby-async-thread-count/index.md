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

So we have the same "run" thread, but we also have a thread that looks like it's realted to the `concurrent-ruby` gem. 





* What is going on in `ruby_thread_local_var:38`
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

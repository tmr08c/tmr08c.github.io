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

#### Quitting

We check for <kbd>q</kbd> or <kbd>x</kbd> as a way to exit the script.

#### List

We accept <kbd>l</kbd> or <kbd>list</kbd> as a way to print the count of Threads the Ruby process knows about. From the docs for [`Thread,list`](https://ruby-doc.org/core-2.5.0/Thread.html#method-c-list):

> Returns an array of Thread objects for all threads that are either runnable or stopped.

Initially, I printed the list, but found I was mostly interested in seeing if the count changed during this exercise.


* follow up to previous post
* Made CLI for interacting
    * link to file
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
- [ ] Add style for `kbd` tag

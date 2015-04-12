---
layout: post
title:  "Are Your Class Methods Actually Private?"
date:   2015-04-09 21:03:02
categories: classes
---

While no methods in Ruby are really safe, even when private, it's possible the class methods you think are private aren't actually private.

## Private Instance Methods

To make an instance method private you call the [`private`](http://ruby-doc.org/core-2.0.0/Module.html#method-i-private) method. This can be done in two ways.

### On it's own line

You can put the `private` method on its own line. When you do this, everything after it will be private.

```ruby
class Pizza

  private

  def remaining_slices
    puts "I'm not sharing."
  end

  def hide
    puts "These aren't the pizzas you're looking for."
  end
end
```

In this case both `remaining_slices` and `hide` will be private since they follow the call to the `private` method.

```ruby
>> pizza = Pizza.new #=> #<Pizza:0x00000102988e58>
>> pizza.remaining_slices
    NoMethodError: private method 'remaining_slices' called for #<Pizza:0x00000102988e58>
>> pizza.hide
    NoMethodError: private method 'hide' called for #<Pizza:0x00000102988e58>
```


### Prefix a Method Definition

You can also prefix specific method definitions with the `private` method. This will make only that method private.

```ruby
class Pizza

  private def remaining_slices
    puts "I'm not sharing."
  end

  def hide
    puts "These aren't the pizzas you're looking for."
  end
end
```

If we create a new instance of our pizza class

```ruby
>> pizza = Pizza.new #=> #<Pizza:0x00000101346168>
```

Our `remaining_slices` method will still be private

```ruby
>> pizza.remaining_slices
  NoMethodError: private method 'remaining_slices' called for #<Pizza:0x00000101346168>
```

but now our `hide` method is public, even though it comes after the private definition of `reamaining_slices`

```ruby
>> pizza.hide 
"These aren't the pizzas you're looking for."
```

## Private Class Methods

Now, it seems like defining a private class method should be the same.

Let's change our `Pizza` class definition to call private when defining a class method:

```ruby
class Pizza
  private

  def self.make_special_recipe
    puts 'A wonderful combination of cheese, carbs, and love'
  end
end
```

Now when we try to call `Pizza.make_special_recipe` we should receive a `NoMethodError` like before, right?

```ruby
>> Pizza.make_special_recipe
  "A wonderful combination of cheese, carbs, and love"
```

It looks like we are able to call the class method, even though we thought we made it private.

If we dig around in the documentation we find that there is actually another private-like method that deals with classes, the not so subtley named,  [`private_class_method`](http://ruby-doc.org/core-2.0.0/Module.html#method-i-private_class_method) method.


We can now update our class definition to prefix our class method definition with our new found `private_class_method` method

```ruby
class Pizza

  private_class_method def self.make_special_recipe
    puts 'A wonderful combination of cheese, carbs, and love'
  end
end
```

and now when we try to call our class method we receive the expected `NoMethodError`

```ruby
>> Pizza.make_special_recipe
  NoMethodError: private method 'make_special_recipe' called for Pizza:Class
```

### Multiple Methods

So `private_class_method` works the same as `private`, just on class methods, right?

If so, we should be able to put it on its own line, and everything below will be private.

```ruby
class Pizza

  private_class_method

  def self.make_special_recipe
    puts 'A wonderful combination of cheese, carbs, and love'
  end

  def self.another_private_method
    puts "You can't see me"
  end
end

>> Pizza.make_special_recipe
"A wonderful combination of cheese, carbs, and love"
>> Pizza.another_private_method
"You can't see me"
```

Now neither method is private!

The issue is `private_class_method` expects to take arguments, symbols, representing the names of methods it is to make private.

This worked in our first example because, when defining a method, Ruby returns that method's name as a sybol

```ruby
>> def foo; end #=> :foo
```

so

```ruby
private_class_method def self.make_special_recipe
  puts 'A wonderful combination of cheese, carbs, and love'
end
```

was equivalent to

```ruby
method_name = def self.make_special_recipe
  puts 'A wonderful combination of cheese, carbs, and love'
end
private_class_method(method_name)
```

Notice that the call to `private_class_method` has to come **after** the method is defined. It will not work if it is passed the method name that is not yet defined.

When `private_class_method` is on it's own line we are essentially passing in `nil` so no class methods are made private.

Now that we undestand how `private_class_method` works and why we can't have `private_class_method` on its own line, what happens if we pass in multiple methods to make private?

```ruby
class Pizza

  def self.make_special_recipe
    puts 'A wonderful combination of cheese, carbs, and love'
  end

  def self.another_private_method
    puts "You can't see me"
  end
  private_class_method :make_special_recipe, :another_private_method
end

>> Pizza.make_special_recipe
  NoMethodError: private method 'make_special_recipe' called for Pizza:Class
>> Pizza.another_private_method
  NoMethodError: private method 'another_private_method' called for Pizza:Class
```

It looks like that works!

We can also pass in the method definitions as arguments and acheieve the same results.

```ruby
class Pizza

  private_class_method(
    def self.make_special_recipe
      puts 'A wonderful combination of cheese, carbs, and love'
    end,

    def self.another_private_method
      puts "You can't see me"
    end
  )
end

>> Pizza.make_special_recipe
  NoMethodError: private method 'make_special_recipe' called for Pizza:Class
>> Pizza.another_private_method
  NoMethodError: private method 'another_private_method' called for Pizza:Class
```

### Alternative Syntax

There is another way we can define private class methods that actually **involves** the `private` method - by using the `class << self` syntax.

```ruby
class Pizza
  class << self
    private

    def make_special_recipe
      puts 'A wonderful combination of cheese, carbs, and love'
    end

    def another_private_method
      puts "You can't see me"
    end
  end
end
```

just like with our previous solutions, attempting to call these methods on our class will result in `NoMethodError`s


```ruby
>> Pizza.make_special_recipe
  NoMethodError: private method 'make_special_recipe' called for Pizza:Class
>> Pizza.another_private_method
  NoMethodError: private method 'another_private_method' called for Pizza:Class
```

Shortly after discovering that `private` doesn't work for class methods the same as it does for instance methods, I discovered some code at work where I was making this mistake. In this case I had a few small classes that had all of their functionality defined in class methods. I found making use of the `class << self` syntax to be a cleaner solution than using `private_class_method` to make them private.

In a [previous post]({% post_url 2015-03-15-the-anatomy-of-a-ruby-class %}) I said I wasn't a big fan of this syntax. Since writing that post, I have become more comfortable with the `class << self` syntax and have been interested in trying to use it in production code. This scenario was definitely a win for the `class << self` syntax in my book.
 
Lately I have been listening to [some](http://bikeshed.fm) of the [podcasts](http://giantrobots.fm) produced by [Thoughtbot](https://thoughtbot.com). They bring up their [Trello](https://trello.com) board of ideas they want to test. The ideas are vetted and then it is decided if they should be used company wide. I was speaking with a coworker about my recent work on a project I hadn't worked on in a while and realzied I sort of do the same thing but not as officailly. I can look through various commits and pick out design patterns and recall what I read or watched that inspired me to try something differently. I forsee the split use of `class << self` and `def self.method` syntaxes to be something I test internally until I settle on one or the other (or have rules for when to use which). 

One of the best and worst parts of programming is that there usaually isn't just one solution to a problem. This can be frustrating when you want to just get something done and have it be considered "right". This can also be a lot fun! I like that I can find a use case for a pattern I haven't used before and see if it applies to my current problem. I also like that in as soon as a week I can come back to the same code and hate that pattern because I have some new insight. In order to excel at programing we need to be pushing ourselves, trying new things, and abandoning old things that don't work. We need to always be evolving.

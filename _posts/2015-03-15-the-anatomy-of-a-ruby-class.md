---
layout: post
title:  "The Anatomy of a Ruby Class"
date:   2015-03-15 21:55:02
categories: classes
---
In this post I am going to break down the basic parts of a Ruby class.

For this example we will look at the following `Cat` class:

```ruby
class Cat
  attr_reader :name
  attr_accessor :hunger_level

  def initialize(name)
    @name = name
    @hunger_level = 10
  end

  def self.says
    'Meow'
  end

  def feed(food_name)
    if hunger_level > 0
      self.hunger_level -= 5
      puts "Thanks for the yummy #{food_name}!"
    else
      puts "I'm not hungry"
    end
  end
end
```

#Creating an Instance of our Class

To create an instance of a Ruby class we use the `new` method, like

```ruby
cat = Cat.new
```

The class' `new` method then calls the `initializee` method.

We can see this when we attempt to create a new `Cat` without passing in any arguments

```ruby
>> Cat.new
ArgumentError: wrong number of arguments (0 for 1)
        from (irb):126:in 'initialize'
        from (irb):148:in 'new'
```

The `new` method calls the `initializee` method which, when called with no arguments, raises an exception.

If we attempt to create a new `Cat` and pass in an argument we are able to successfully create a new instance of a `Cat`

```ruby
Cat.new('Mr. Whiskers')
#=> #<Cat:0x00000104833f10 @name="Mr. Whiskers", @hunger_level=10>
```

#Instance Variables

We see that our new object has `@name` and `@hunger_level` attributes. The `@` variables are known as instance variables. These are variable associated with only that object or that *instance* of the class.

If we look in the initialize method we can see how we end up with our values of `@name` and `@hunger_level`


```ruby
def initialize(name)
  @name = name
  @hunger_level = 10
end
```

`@name` is set to the parameter we pass in. In the above example we called `new` with the parameter `"Mr. Whiskers"` which is why our `Cat` has a `@name` of "Mr. Whiskers".

`@hunger_level` is set to a value of `10` every time which is why our new `Cat` has a `@hunger_level` equal to `10`.

##Accessing Instance Variables

###Getting

In order to give users access to our instance variable we need to provide a method call that will return the value.

For example, if we want to provide a "getter" for our `@name` variable we can define a method like

```ruby
def name
  @name
end
```

and we can now call that method

```ruby
cat.name
#=> "Mr. Whiskers"
```

This getter logic is so common Ruby provides us with a shortcut, `attr_reader`.

```ruby
attr_reader :name
```

With `attr_reader` we have no need to define our `name` method anymore and will have the same results.

```ruby
cat.name
#=> "Mr. Whiskers"
```

###Setting

As you may be able to imply from the name, `attr_reader` only allows the ability to read the value, not set it.

Attempting to set the `name` attribute will result in a `NoMethodError` exception

```ruby
>> cat.name = 'Bob'
NoMethodError: undefined method 'name=' for #<Cat:0x00000104833f10 @name="Mr. Whiskers", @hunger_level=10>
```

This error points us in the direction of how to add the ability to set the `@name` value by letting us know the `name=` method is undefined.

Let's reopen our `Cat` class and add the `name=` method:

```ruby
class Cat
  def name=(name)
    @name = name
  end
end
```

Now if create a new `Cat` we can get **and** set it's name:

```ruby
cat = Cat.new("Mr.Whiskers")
#=> #<Cat:0x000001033d6ec8 @name="Mr.Whiskers", @hunger_level=10>
cat.name
#=> "Mr.Whiskers"
cat.name = 'Benjamin Cat'
#=> "Benjamin Cat"
cat
#=> #<Cat:0x000001033d6ec8 @name="Benjamin Cat", @hunger_level=10>
```

Rather than having to define the `name=` method Ruby has a shortcut similar to `attr_reader`, it is `attr_writer`.

We can change our `Cat` class definition to look like

```ruby
class Cat
  attr_reader :name
  attr_writer :name
end
```

and remove our definition of `name=`.

###Both

The need for getters and setters, or `attr_reader`s and `attr_writer`s, is so common, Ruby has a method that will do both at the same time, `attr_accessor`.

Our original definition of the `Cat` has an `attr_accessor` for the `@hunger_level` instance variable

```ruby
class Cat
  attr_accessor :hunger_level
end
```

this allows us to set and get the `@hunger_level` instance variable without defining the `hunger_level` or `hunger_level=` methods.

```ruby
cat = Cat.new('Sally Cat')
#=> #<Cat:0x0000010227bae8 @name="Sally Cat", @hunger_level=10>
cat.hunger_level
#=> 10
cat.hunger_level = 12
#=> 12
cat
#=> #<Cat:0x0000010227bae8 @name="Sally Cat", @hunger_level=12>
cat.hunger_level
#=> 12
```

#Class Methods

Some methods can be called directly on a class, these are known as class methods.

Class methods are method definitions that are prefixed with `self`

In our above example we have a class method, `says`

```ruby
def self.says
  'Meow'
end
```

which can be sent to the `Cat` class directly

```ruby
Cat.says #=> "Meow"
```

##Alternative Class Method Definition

There is an alternative syntax for defining class methods, `class << self`

```ruby
class Cat
  attr_reader :name
  attr_accessor :hunger_level

  def initialize(name)
    @name = name
    @hunger_level = 10
  end

  # Class methods go here
  class << self
    def says
      'Meow'
    end
  end

  def feed(food_name)
    if hunger_level > 0
      self.hunger_level -= 5
      puts "Thanks for the yummy #{food_name}!"
    else
      puts "I'm not hungry"
    end
  end
end
```

I prefer the `self.method_name` syntax over the `class << self` syntax. I like the ability to look at any method and know right away wether it is a class method or not.

There are, however, reasons for the `class << self` syntax. For example, from [this Stackoverlow Post](http://stackoverflow.com/a/10964198):

> `class << self` is good at keeping all of your class methods in the same block. If methods are being added in `def self.method` form then there's no guarantee (other than convention and wishful thinking) that there won't be an extra class method tucked away later in the file.

I see this reasoning as potentially being beneficial, especially in some larger classes we have in our applications at work. However, when there are so many method you are hunting around and have various types of method all about the class this may be a code smell in and of itself and a sign some additional objects may need to be created (as is probably the case for the work example I am thinking of).

#Instance Methods

Method defined without `self` (or not in a `class << self` block) are known as instance methods. These methods can be called on instances of a given class.

In our `Cat` example above we have an instance method, `feed`, which takes one argument, `food_name`:

```ruby
def feed(food_name)
  if hunger_level > 0
    self.hunger_level -= 5
    puts "Thanks for the yummy #{food_name}!"
  else
    puts "I'm not hungry"
  end
end
```

This method will check the cat's `@hunger_level` variable (via the `attr_accessor`) and if it is above `0` will reduce the cat's hunger level and print out a thank you message. If the cat's `@hunger_level` is `0` (or below) the method will simply print out *I'm not hungry*.

Again, this method can **not** be called on the `Cat` class itself

```ruby
>> Cat.feed('pizza')
NoMethodError: undefined method `feed' for Cat:Class
        from (irb):26
        from /Users/troyrosenberg/.rvm/rubies/ruby-2.1.1/bin/irb:11:in `<main>'
```

but rather must be called on an *instance* of the `Cat` class

```ruby
>> cat = Cat.new('Mr. Whiskers')
>> cat.feed('pizza')
"Thanks for the yummy pizza!"
>> cat.feed('chips')
"Thanks for the yummy chips!"
>> cat.feed('carrots')
"I'm not hungry" # convenient, eh?
```

##Scoping

I would like to point out something about the way we access `@hunger_level` via our `attr_accessor` helper methods.

### Getting

When we attempt to retrieve our `@hunger_level` value we are able to simply call `hunger_level`,

```ruby
if hunger_level > 0
```

Ruby will first look for a local variable named `hunger_level`. Since there is no local variable it will see if the class can respond to the `hunger_level` message, which it can, thanks to `attr_accesor`. Therefore it will end up returning the value of `@hunger_level`.


### Setting

When attempting to set the value of `@hunger_level` we actually to do so something slightly different than simply call `hunger_level -= 5`.

```ruby
self.hunger_level -= 5
```

Here we have to use `self` and for a different reason than for defining a class method.

Within an instance method, `self` refers to the instance object that the method is being called upon.

To demonstrate this we can define an additional method in the `Cat` class:

```ruby
class Cat
  def who_is_self
    puts self.inspect
  end
end
```

This method will print `self.inspect` which will provide us with some insight as to what `self` is.

We can now create a new `Cat`

```ruby
cat = Cat.new("Fluffy") #=> #<Cat:0x0000010394a580 @name="Fluffy", @hunger_level=10>
```

Note the object's memory reference, `0x0000010394a580`.

Now we can call our `who_is_self` method on our `cat`:

```ruby
>> cat.who_is_self
#<Cat:0x0000010394a580 @name="Fluffy", @hunger_level=10>
```

We see that inspecting `self` shows that self is the `cat` object itself, same memory location and everything!

Therefore, `self.hunger_level=` is sending the `hunger_level=` message to `self`, or our instance object `cat`.

The reason we have to specify where to send this message is due to the scoping of variables.

As mentioned above, Ruby will first look for a local variable. This is fine when getting the value of a variable because if it does not exist Ruby will move up the scoping tree. However, when setting the value of a variable, if the variable does not exist, Ruby will assume you want to create a new local variable, and set the value that way.

Let's see what happens if we do not use `self.hunger_level`, and instead just use `hunger_level`. I have also added some additional print statements to help clarify what is happening.

```ruby
class Cat
  def feed(food_name)
    if hunger_level > 0
      hunger_level -= 5
      puts "self.hunger_level: #{self.hunger_level}"
      puts "hunger_level: #{hunger_level}"
      puts "Thanks for the yummy #{food_name}!"
    else
      puts "I'm not hungry"
    end
  end
end
```

Now when we try to use the `cat.feed` example like before we receive an exception


```ruby
>> cat.feed("chick'un sandwhich")
NoMethodError: undefined method `-' for nil:NilClass
        from (irb):116:in `feed'
        from (irb):128
        from /Users/troyrosenberg/.rvm/rubies/ruby-2.1.1/bin/irb:11:in `<main>'
```

Since

```ruby
hunger_level -= 5
```

is the same as

```ruby
hunger_level = hunger_level - 5
```

this error is saying our second `hunger_level`, the `hunger_level - 5` part is `nil`.

It seems when Ruby is getting the value of a variable that exists elsewhere in the equation the scoping rules are different. When trying to get the value of `hunger_level` to subtract `5`, it seems to realize we are also setting a `hunger_level` variable and this this new, local variable, needs to be created.

We can try again using `self.hunger_level` to get the value and see what happens with our setter

```ruby
class Cat
  def feed(food_name)
    if hunger_level > 0
      hunger_level = self.hunger_level - 5
      puts "self.hunger_level: #{self.hunger_level}"
      puts "hunger_level: #{hunger_level}"
      puts "Thanks for the yummy #{food_name}!"
    else
      puts "I'm not hungry"
    end
  end
end


>> cat = Cat.new("Katty") #=> #<Cat:0x000001011ba6f0 @name="Katty", @hunger_level=10>
>> cat.feed('fish') #=> nil
self.hunger_level: 10
hunger_level: 5
Thanks for the yummy fish!
>> cat.feed('fish') #=> nil
self.hunger_level: 10
hunger_level: 5
Thanks for the yummy fish!
```

This time we do not receive an exception but we see are changing the `@hunger_level` but rather creating a new local variable each time.

# Conclusion

We have covered some basics and some intricate details of a basic Ruby class and should now understand exactly what is going on in our `Cat` class definition.

```ruby
class Cat
  attr_reader :name
  attr_accessor :hunger_level

  def initialize(name)
    @name = name
    @hunger_level = 10
  end

  def self.says
    'Meow'
  end

  def feed(food_name)
    if hunger_level > 0
      self.hunger_level -= 5
      puts "Thanks for the yummy #{food_name}!"
    else
      puts "I'm not hungry"
    end
  end
end
```

Hopefully this can be useful for helping you decipher some Ruby code and making writing your own code easier.

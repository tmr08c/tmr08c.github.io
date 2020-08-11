---
title: 'Initial Hash Value in Ruby'
date: '2020-08-04T06:08:02.123Z'
categories: ['ruby']
---

Ruby's [`Hash.new`](https://ruby-doc.org/core-2.7.1/Hash.html#method-c-new) has three different options for initialization. These options differ in how they handle missing keys. In this post, we will cover the three different styles as well as some potential issues you could run into.

## tl;dr

Before we cover the different options more in depth, here's an overview of the different options that can be used as a reference:

```ruby
# == No arguments, default to `nil`
new_hash = Hash.new
=> {}

> new_hash[:not_here]
=> nil

> bare_hash = {}
=> {}

> bare_hash[:also_not_here]
=> nil

# == Argument, use as default
> hash_with_default = Hash.new(:default)
=> {}

> hash_with_default[:not_set]
=> :default

# WARNING! This will use the **same** object
> h = Hash.new([])
> h[:first] <<= 1
> h[:second] <<= 2

> h
=> {:first=>[1, 2], :second=>[1, 2]}

# == Using a block
> h = Hash.new { |hash, key| hash[key] = [] }

# reference a key that doesn't exist
> h[:izzo]

> h
=> {:izzo=>[]}
```

## No Argument

The primary way most people create hashes is not actually using `Hash.new`, but instead created a new hash directly with the `{}` syntax. For the purposes of handling missing keys, this behaves the same as `Hash.new`.

When you use `Hash.new` with no arguments (or the `{}` syntax). The hash will return `nil` if a key does not exist.

```ruby
new_hash = Hash.new
=> {}

> new_hash[:not_here]
=> nil

> bare_hash = {}
=> {}

> bare_hash[:also_not_here]
=> nil
```

Aside from the [evils of `nil`](https://thoughtbot.com/blog/if-you-gaze-into-nil-nil-gazes-also-into-you), this works most of the time. Add in some `if`s or the [safe navigation operator](https://ruby-doc.org/core-2.6/doc/syntax/calling_methods_rdoc.html#label-Safe+navigation+operator) and you're all set!

## With an Argument

The second form of initializing a Hash with by passing in an object. This object will then be used as the default value. From the docs:

> If obj is specified, this single object will be used for all default values.

```ruby
> hash_with_default = Hash.new(:default)
=> {}

> hash_with_default[:set] = :not_default

> hash_with_default[:set]
=> :not_default
> hash_with_default[:not_set]
=> :default
```

### Use Case

The most common way I reach for this is when I am using my hash as a counter.

For example, let's say we want to count the number of each type of letter in a sentence. Without a default value you would have to do something lke:

```ruby
sentence =
  "to be, or not to be"

letter_counter = Hash.new

sentence.each_char do |letter|
  letter_counter[letter] ||= 0
  letter_counter[letter] += 1
end

> letter_counter
=> {"t"=>3, "o"=>4, " "=>5, "b"=>2, "e"=>2, ","=>1, "r"=>1, "n"=>1}
```

By setting a default value of `0`, you would no longer need to do `letter_counter[letter] ||= 0`, because if a key doesn't exist, we get a default value of `0`.

```ruby
sentence =
  "to be, or not to be"

letter_counter = Hash.new(0)

sentence.each_char do |letter|
  letter_counter[letter] += 1
end

> letter_counter
=> {"t"=>3, "o"=>4, " "=>5, "b"=>2, "e"=>2, ","=>1, "r"=>1, "n"=>1}
```

While it's only a one line difference in this example, it helps remove some of the ceremony and enables more focus on what the code is actually trying to do.

### Mutable Objects

When using the argument version of `Hash.new` there is a phase in the documentation you need to keep in mind (emphasis mine):

> If obj is specified, this **single object** will be used for all default values.

The **single object** passed in will be (re)used for all defaults. In our example above we use an integer. These are not mutable, `0` will always be `0` when using it as the default. However, if you use something that can be mutated, you will see the impact of this single object re-use.

```ruby
company = Hash.new([])

# If you haven't seen `<<=` it's a form of
# [abbreviate assignment](https://ruby-doc.org/core-2.7.1/doc/syntax/assignment_rdoc.html#label-Abbreviated+Assignment)
# similar to `+=` above, but with the shovel operator
company[:development] <<= "dev1"
company[:marketing] <<= "marketer1"
company[:development] <<= "dev2"
company[:hr] <<= "hr-rep1"
company[:hr] <<= "hr-rep2"

company
=> {
  :development => ["dev1", "marketer1", "dev2", "hr-rep1", "hr-rep2"],
  :marketing =>   ["dev1", "marketer1", "dev2", "hr-rep1", "hr-rep2"],
  :hr =>          ["dev1", "marketer1", "dev2", "hr-rep1", "hr-rep2"]
}
```

What's going on here? Every hash value looks the same!?

This is the result of using the **single object** for all default values. When we are setting an new key in the hash we set it to default to an array. However, rather than getting a new array each time, we get the **same** array. Another way to think about this could be something like:

```ruby
default_value = []

Hash.new(default_value)
```

With it written this way, the behavior may be less surprising. When you pass in a variable, it seems more intuitive that the same variable would be used.

You will see similar behavior with most other objects you use a default and should be aware of whether that is the behavior you wnt or not.

So, is it possible to use something like an array as your default value? That brings us to our third and final option for setting defaults with `Hash.new`, the block syntax.

### Alternative default value syntax

While the focus of this post is about leveraging `Hash.new`, there is an alternative way to get this same functionality. Ruby also provides a [`Hash#default=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default-3D) method. This works the same as passing in an object to `#new` and will re-use the single object passed in. One advantage of using the `#default=` method is that you can use it with hashes created using the implicit form (`{}`).

```ruby
> h = {}
> h.default = []

> h[:first] <<= 1
> h[:second] <<= 2

> h
=> {:first=>[1, 2], :second=>[1, 2]}
```

You could also change it if you wanted:

```ruby
> h = {}

> h.default = 0
> h[:first] += 1

> h.default = []
> h[:second] <<= 2

> h
=> {:first=>1, :second=>[2]}
```

It's possible this is more of a [footgun](https://en.wiktionary.org/wiki/footgun), than something you want to do in practice.

In general, I think there is value in co-locating the creation of the hash with the the default value for ease of understanding and debugging. Setting the default in other places can make it harder to track down what the expected behavior should be.


## Using a block

The third option for setting a default argument with `Hash.new` is to pass it a block. When you try to access a key that does not exist, this block will be executed and will return whatever is returned in the block. Let's see what this looks like:

```ruby
h = Hash.new do
  puts "it looks like you don't have this key yet. Let's set it to an empty array"
  []
end

> h[:new]
it looks like you don't have this key yet. Let's set it to an empty array
=> []
```

Now that we are defaulting to an empty array, do we run into the same problems we had before?

### Returning a value

```ruby
> h[:new]
it looks like you don't have this key yet. Let's set it to an empty array
=> []

# should we have our `new` key now? 
> h
# it doesn't look like it...
=> {}

# maybe we need to set it to something? 
> h[:new] << 1
it looks like you don't have this key yet. Let's set it to an empty array
=> [1]

# ...still doesn't seem to stick around
> h
=> {}
>
```

It looks like our block is being run and returning an empty array, but it isn't actually setting up the key/value pair in the hash. One way around this is the `<<=` operator we saw before.

```ruby
h[:new] <<= 1
```

This can be thought of as:

```ruby
h[:new] = h[:new] << 1
```

The right side version of `h[:new]` will be run first. This will run our block which will return our empty array.

```ruby
h[:new]
# => []
```

We then add `1` onto our array.

```ruby
h[:new] << 1
# => [1]
```

Our right side is now `[1]`.

We will then set `h[:new]` equal to the right side, which is our array. 

```ruby
h[:new] = [1]

> h
=> {:new=>[1]}
```

Now that we are able to actually _set_ things in our hash, do we run into the same problem where all of our values look the same? 

```ruby
> h[:first] <<= 1
it looks like you don't have this key yet. Let's set it to an empty array
=> [1]
> h[:second] <<= 2
it looks like you don't have this key yet. Let's set it to an empty array
=> [2]
> h[:first] <<= 3
=> [1, 3]

> h
=> {:first=>[1, 3], :second=>[2]}
```

We no longer have this problem! This is because rather than sharing the same object as our default value, we are running the block each time and creating a _new_ array every time.

Even though our problem of sharing the same value is gone, it is a bit unintuitive that we have to use the `<<=` operator. I would expect to be able to simply shovel in a new value and it update my hash without me having to go through the extra steps. 

Fortunately, we can make this happen!

### Updating the hash

In our previous example, our block was returning our default value, but not setting the key in our hash. Let's see if there's anything in the documentation that helps us:

> If a block is specified, it will be called with the hash object and the key,
> and should return the default value. It is the block's responsibility to
> store the value in the hash if required.

The documentation notes the block is responsible for storing the value in the hash. Since we weren't explicitly doing that in our previous block, it seems it's expect behavior that the values aren't set. But how do we set them? 

Something else the documentation points out is the block will be passed the hash and they key requested. Previously, we weren't capturing the arguments passed into the hash. Below is an example where we capture the arugments and print out some information about them.

```ruby
# in the block we receive `hash` and `key`
h = Hash.new do |hash, key|
  # print some information about our arguments
  puts "Currently, your hash looks like #{hash}. " \
       "This does not include the key '#{key}'. " \
       "We will set it to a default empty array"

  # do what we did before and simply return the array
  []
end
```

Let's take a look at what this looks like when we try to set a access a key that doesn't exist:

```ruby
> h[:old] = :set
=> :set

> h[:new]
  Currently, your hash looks like {:old=>:set}. \
  This does not include the key 'new'. \
  We will set it to a default empty array
=> []

# we stil have the old key,
# but didn't set te new one
> j
=> {:old=>:set}
```

### Storing a value

From this example, we can see we are passed in the hash we working with and the key we are attempting to access (that doesn't yet exist). We continued to simply return the new array, but what if we take the advice of the documentation and try to store the value in the hash if that's what we want? 


```ruby
# set `hash`'s `key` to equal our default empty array
> h = Hash.new { |hash, key| hash[key] = [] }
=> {}

# reference a key that doesn't exist
> h[:izzo]
=> []

# confirm that it is still set
> h
=> {:izzo=>[]}
```

Does this also avoid our problem of re-using the same value?

```ruby
> h[:first] << 1
> h[:second] << 2
> h[:first] << "one"
> h[:first] << "uno"

> h
=>  :first=>[1, "one", "uno"], :second=>[2]}
```

It does! Again, we are seeing that running the block creates a _new_ array every time the block is executed instead of reusing the same instance like we saw with the parameter version.

### Alternative default block syntax

Similar to `Hash#default=` covered [above](#alternative-default-value-syntax), there is a [`Hash#default_proc=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default_proc-3D) method that can be used to set a default value for a hash using a proc.

```ruby
> h.default_proc = proc { |hash, key| hash[key] = [] }
=> #<Proc:0x00007f98620de268@(>
> h[:first] << 1
=> [1]
irb(main):082:0> h
=> {:first=>[1]}
```

One thing to point out is that the `default=` method covered above will **not** work if given a proc:

```ruby
> h.default = proc { |hash, key| hash[key] = [] }

# instead of getting back an array, 
# we get a Proc
> h[:first] << 1
=> #<Proc:0x00007f98620c8120>

# and nothing is set in our hash
> h
=> {}
```

It also looks like Ruby will only let you have `default` or `default_proc` set and will clear out the other when one is set.

```ruby
> h = {}

# check initial defaults
> h.default
=> nil
> h.default_proc
=> nil

# set default
> h.default = 0
> h.default
=> 0

# set default_proc
> h.default_proc = proc { [] } 
=> #<Proc:0x00007f986313bc00@(>

# default is now nil
> h.default
=> nil
> h.default_proc
=> #<Proc:0x00007f986313bc00@(>

# set default again
> h.default = 1

# default_proc is now nil
> h.default_proc
=> nil
> h.default
=> 1
```

## Conclusion

In the post we've covered the three different default value options when initializing the hash - give no arguments and defaulting to `nil`, giving an argument and use that **same** object for the value each time, and using a block set the value of the new key.

We also covered any gotchas you may run into with each of these options so you can avoid problems when using these options in your code.


* Returning just an array
* The SO answer where you return the whole hash `hsh2 = Hash.new { |hash, key| hash[key] = [] }`

# TODO

- [ ] Maybe look into [`default=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default-3D)
    - https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default-3D will act like the argument
    - https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default_proc-3D will act like the block
- [ ] Maybe a tl;dr at the top? 

---
date: '2020-08-27T06:28:02'
slug: initialize-hash-value-in-ruby
tags:
- ruby
title: Initial Hash Value in Ruby
---

Ruby's [`Hash.new`](https://ruby-doc.org/core-2.7.1/Hash.html#method-c-new) has three different options for initialization. These options differ in how they handle missing keys. In this post, we will cover the three variations and issues you may encounter using them.

## tl;dr

Before we cover the different options more in-depth, here's an overview.

```ruby
# == No arguments, default to `nil`
new_hash = Hash.new
new_hash[:not_here]
=> nil

# same behavior with `{}` style
bare_hash = {}
bare_hash[:also_not_here]
=> nil

# == Argument, use as default
hash_with_default = Hash.new(:default)
hash_with_default[:not_set]
=> :default

# WARNING! This will use the **same** object
h = Hash.new([])
h[:first] <<= 1
h[:second] <<= 2
=> {:first=>[1, 2], :second=>[1, 2]}

# == Using a block
h = Hash.new { |hash, key| hash[key] = [] }
h[:first] <<= 1
h[:second] <<= 2
=> {:first=>[1], :second=>[2]}
```

## No Argument

Most of the time you probably don't actually use `Hash.new` for creating a new hash. Instead, you are probably used to using the implicit form syntax (`{}`). For the purposes of handling missing keys, this behaves the same as `Hash.new` when no argument is passed in.

When you use `Hash.new` with no arguments (or the `{}` syntax) the hash will return `nil` if a key does not exist.

```ruby
new_hash = Hash.new
new_hash[:not_here]
=> nil

bare_hash = {}
bare_hash[:also_not_here]
=> nil
```

Aside from the [evils of `nil`](https://thoughtbot.com/blog/if-you-gaze-into-nil-nil-gazes-also-into-you), this works for most situations. By adding in some `if`s or using the [safe navigation operator](https://ruby-doc.org/core-2.6/doc/syntax/calling_methods_rdoc.html#label-Safe+navigation+operator) you can successfully work with the default of `nil`.

Sometimes, however, it can be easier to work with your code if you have a default value returned when accessing a key that doesn't exist. Let's see how this can be done.

## With an Argument

The second form of initializing a Hash is by passing in an object. This object will then be used as the default value. [From the docs](https://ruby-doc.org/core-2.7.1/Hash.html#method-c-new):

> If obj is specified, this single object will be used for all default values.

```ruby
hash_with_default = Hash.new(:default)
hash_with_default[:set] = :not_default

hash_with_default[:set]
=> :not_default
hash_with_default[:not_set]
=> :default
```

### Use Case

I most commonly reach for this style when using a hash as a counter.

For example, let's say we want to count the occurrences of each letter in a sentence. Without a default value you would have to do something like:

```ruby
sentence =
  "to be, or not to be"

letter_counter = Hash.new

sentence.each_char do |letter|
  letter_counter[letter] ||= 0
  letter_counter[letter] += 1
end

letter_counter
=> {"t"=>3,
    "o"=>4,
    " "=>5,
    "b"=>2,
    "e"=>2,
    ","=>1,
    "r"=>1,
    "n"=>1}
```

By setting a default value of `0`, you would no longer need to do `letter_counter[letter] ||= 0`. If a key doesn't exist, we get a default value of `0`.

```ruby
sentence =
  "to be, or not to be"

letter_counter = Hash.new(0)

sentence.each_char do |letter|
  letter_counter[letter] += 1
end

letter_counter
=> {"t"=>3,
    "o"=>4,
    " "=>5,
    "b"=>2,
    "e"=>2,
    ","=>1,
    "r"=>1,
    "n"=>1}
```

While it's only a one-line difference in this example, it helps remove some of the ceremony and enables more focus on what the code is actually trying to do.

### `<<=` Operator

Before we move onto the next section, I want to provide a quick introduction to the `<<=` operator because we will be using it in upcoming examples.

The `<<=` operator is an example of [abbreviated assignment](https://ruby-doc.org/core-2.7.1/doc/syntax/assignment_rdoc.html#label-Abbreviated+Assignment) - this is similar to operators like `+=` and `||=`.

This operator is necessary when we return a default value for a hash but don't _set_ that value in the hash.

```ruby
# default to returning an array
h = Hash.new([])

# for a key that doesn't exist, we get back an array
h[:foo]
=> []

# but it's not set in the hash itself yet
h
=> {}
```

With the `<<=` operator, we can shovel (`<<`) a value onto the default array that is returned **and** set the hash value equal to the resulting array. Let's break it down:

```ruby
h[:new] <<= 1
```

This can be thought of as:

```ruby
h[:new] = h[:new] << 1
```

The right side version of `h[:new]` will be run first. Since the key doesn't exist, it will use the default value we passed in (the array).

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

h
=> {:new=>[1]}
```

Let's see how we can leverage this operator to more easily interact with our new default return values.

### Mutable Objects

When using the argument version of `Hash.new` there is a phrase in the documentation we need to keep in mind (emphasis mine):

> If obj is specified, this **single object** will be used for all default values.

The **single object** passed in will be (re)used for all defaults. In our example above we used an integer. These are not mutable, so `0` will always be `0` when using it as the default. However, if you use something that can be mutated, you will see the impact of this single object re-use.

```ruby
company = Hash.new([])

company[:development] <<= "dev1"
company[:marketing] <<= "marketer1"
company[:development] <<= "dev2"

company
=> {
  :development => ["dev1", "marketer1", "dev2"],
  :marketing =>   ["dev1", "marketer1", "dev2"]
}
```

What's going on here? Both hash value looks the same!?

This is the result of using the **single object** for all default values. When we are setting a new key in the hash we set it to default to an array. However, rather than getting a new array each time, we get the **same** array. Another way to think about this could be something like:

```ruby
default_value = []
Hash.new(default_value)
```

When written this way, the behavior may be less surprising. When you pass in a variable, it may seem more intuitive that the same variable would be used.

You will see similar behavior with most other objects you use as a default in this way and should be aware of whether that is the behavior you want or not.

### Alternative default value syntax

While the focus of this post is about leveraging `Hash.new`, there is an alternative way to get this same functionality. Ruby also provides a [`Hash#default=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default-3D) method. This works the same as passing in an object to `#new` and will re-use the single object passed in.

One advantage of using the `#default=` method is that you can use it with hashes created using the implicit form (`{}`).

```ruby
h = {}
h.default = []

h[:first] <<= 1
h[:second] <<= 2

=> {:first=>[1, 2],
    :second=>[1, 2]}
```

You can also change the default multiple times if you want, though this may be more of a [footgun](https://en.wiktionary.org/wiki/footgun) than something you want to do in practice.

```ruby
h = {}

h.default = 0
h[:first] += 1

h.default = []
h[:second] <<= 2

=> {:first=>1,
    :second=>[2]}
```

In general, I think there is value in co-locating the creation of the hash with the default value. This makes it easier to understand and debug. Setting the default in (multiple) other places can make it harder to track down what the expected behavior should be. Although, you can use [`Hash#default`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default) to check the current default value to help with this.

## Using a block

The third option for setting a default argument with `Hash.new` is to pass it a block. When you try to access a key that does not exist, this block will be executed and will return whatever is returned in the block. Let's see what this looks like:

```ruby
h = Hash.new do
  puts "It looks like you don't have this key yet." \
        " Let's set it to an empty array"
  []
end

h[:new]
"It looks like you don't have this key yet.
Let's set it to an empty array"
=> []
```

### Returning a value

Does returning an empty array from the block help solve the problem of object reuse?

```ruby
h = Hash.new { [] }

h[:first] <<= 1
h[:second] <<= 2
h[:first] <<= 3

=> {:first=>[1, 3],
    :second=>[2]}
```

We no longer have this problem!

This is because rather than sharing the same object as our default value, we are invoking the block and creating a **new** array any time we do not already have a key.

Even though our problem of sharing the same value is gone, it is a bit unintuitive that we have to use the `<<=` operator. I would expect to be able to use the plain shovel operator (`<<`) to add a value to our empty array.

Fortunately, we can make this happen.

### Updating the hash

In our previous example, our block was returning our default value, but not setting the key in our hash. Let's see if there's anything in the [documentation](https://ruby-doc.org/core-2.7.1/Hash.html#method-c-new) that helps us (emphasis mine):

> If a block is specified, it will be called with the hash object and the key,
> and should return the default value. **It is the block's responsibility to
> store the value in the hash if required.**

The documentation notes the block is responsible for storing the value in the hash. Since we weren't explicitly doing that in our previous block, it seems it's expected behavior that the values aren't set. But how do we set them?

Something else the documentation points out is that the block will be passed the hash and the requested key. Previously, we weren't capturing the arguments passed into the hash. Below is an example where we capture the arguments and print out some information about them.

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

Let's take a look at what this looks like when we try to access a key that doesn't exist:

```ruby
h[:old] = :set
=> :set

h[:new]
  "Currently, your hash looks like {:old=>:set}. \
  This does not include the key 'new'. \
  We will set it to a default empty array"
=> []

# we still have the old key,
# but didn't set the new one
h
=> {:old=>:set}
```

From this example, we can see we are passed in the hash we are attempting to index into and the key we are attempting to access (that doesn't yet exist). We continued to simply return the new array, but what if we take the advice of the documentation and try to store the value in the hash if that's what we want?

### Storing a value

In this example, we will now update our hash and set the key-value pair in our block.

```ruby
# set `hash`'s `key` to equal our default empty array
h = Hash.new do |hash, key|
  hash[key] = []
end

# reference a key that doesn't exist
h[:izzo]
=> []

# it's now set in our hash
h
=> {:izzo=>[]}
```

Does this continue to avoid our problem of reusing the same value?

```ruby
h[:first] << 1
h[:second] << 2
h[:first] << "one"

h
=>  :first=>[1, "one"], :second=>[2]}
```

It does! Again, we are seeing that running the block creates a **new** array every time the block is executed instead of reusing the same instance as we saw with the parameter version.

In addition to setting the key-value pair in our hash, we also return the value. This enables us to interact with the newly set value right away with an operator like `<<`.

With this, we get the ease of use of directly returning a default value without having to remember to update the hash itself.

### Alternative default block syntax

Similar to `Hash#default=` covered [above](#alternative-default-value-syntax), there is a [`Hash#default_proc=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default_proc-3D) method that can be used to set a default value for a hash using a proc.

```ruby
h = {}
h.default_proc = proc do |hash, key|
  hash[key] = []
end

h[:first] << 1

h
=> {:first=>[1]}
```

One thing to point out is that the `default=` method covered above will **not** work if given a proc and you must use the `default_proc=` version (and vice versa for non-procs).

```ruby
h.default = proc { |hash, key| hash[key] = [] }

# instead of getting back an array,
# we get a Proc
h[:first] << 1
=> #<Proc:0x00007f98620c8120>

# and nothing is set in our hash
h
=> {}
```

It also looks like Ruby will (reasonably) only let you have `default` or `default_proc` set. Setting one will clear out the other.

```ruby
h = {}

# check initial defaults
h.default
=> nil
h.default_proc
=> nil

# set default
h.default = 0
h.default
=> 0

# set default_proc
h.default_proc = proc { [] }
=> #<Proc:0x00007f986313bc00@(>

# default is now nil
h.default
=> nil
h.default_proc
=> #<Proc:0x00007f986313bc00@(>

# set default again
h.default = 1

# default_proc is now nil
h.default_proc
=> nil
h.default
=> 1
```

## Checking for existence

One situation to be careful of when setting a default value is using `if` to check if a key exists. This is a fairly common pattern you may see when not setting a default because `nil` is false-y and the `if` will not pass. However, when we set a default value, our `if` will now pass (assuming the default set is truth-y). This may result in unexpected behavior.

```ruby
h = Hash.new { |h,k| h[k] = [] }

# `:foo` key does not exist
if h[:foo]
  puts 'here'
end
# however, since we set the default
# for any key we reference, it's created
# in our `if` and will print "here"
"here"
```

Instead, consider using [`Hash#key?`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-key-3F) to check if the key already exists in the hash without triggering the default value behavior.

```ruby
h = Hash.new { |h,k| h[k] = [] }

h.key?(:bar)
=> false
```

## Conclusion

In this post, we've covered the three different default value options when initializing a hash - (1) giving no arguments and defaulting to `nil`, (2) giving an argument and using that **same** object for the value each time, and (3) using a block set the value of the new key.

We also covered any gotchas you may run into with each of these options so you can avoid problems when using these options in your code.

I hope that this helps make it easy to choose which version of `Hash.new` to use.

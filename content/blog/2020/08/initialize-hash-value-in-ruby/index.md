---
title: 'Initial Hash Value in Ruby'
date: '2020-08-04T06:08:02.123Z'
categories: ['ruby']
---

Ruby's [`Hash.new`](https://ruby-doc.org/core-2.7.1/Hash.html#method-c-new) has three different options for initialization. These options differ in how they handle missing keys. In this post, we will cover the three different styles as well as some potential issues you could run into.

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

With it written this way, the behavior may be less surprising. When you pass in a variable, it seems more intutive that the same variable would be used.

You will see similar behavior with most other objects you use a default and should be aware of whether that is the behavior you wnt or not.

So, is it possible to use something like an array as your default value? That brings us to our third and final option for setting defaults with `Hash.new`, the block syntax.

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

One thing to notice is that our hash isn't actually updated: 
Now that we are defaulting to an empty array, do we run into the same problems we had before?

```ruby
> h[:new]
it looks like you don't have this key yet. Let's set it to an empty array
=> []

# should we have our `new` key now? 
> h
# ...it doesn't look like it...
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

We no longer have this problem! This is because rather than sharing the same object as our default value, we are running the block each time and creating a _new_ array everytime.

Even though our problem of sharing the same value is gone, it is a bit unintuitive that we have to use the `<<=` operator. I would expect to be able to simply shovel in a new value and it update my hash without me having to go through the extra steps. 

Fortunately, we can make this happen!


```ruby
> h
=> {}
```

```ruby
> j = Hash.new do |hash, key|
irb(main):009:1* puts "Currently, your hash looks like #{h}. This does not include the key #{key}. We will set it to a default empty
array"
irb(main):010:1> []
irb(main):011:1> end
=> {}
irb(main):012:0> j
=> {}
irb(main):013:0> j[:new]
Currently, your hash looks like {}. This does not include the key new. We will set it to a default empty array
=> []
irb(main):014:0> j[:old] = :set
=> :set
irb(main):015:0> j
=> {:old=>:set}
irb(main):016:0> j[:new]
Currently, your hash looks like {}. This does not include the key new. We will set it to a default empty array
=> []
irb(main):018:0> j
=> {:old=>:set}
irb(main):019:0> j[:new]
Currently, your hash looks like {}. This does not include the key new. We will set it to a default empty array
=> []
irb(main):020:0> j = Hash.new do |hash, key|
irb(main):021:1* puts "Currently, your hash looks like #{hash}. This does not include the key '#{key}'. We will set it to a default e
mpty array"
irb(main):022:1> end
=> {}
irb(main):023:0> j
=> {}
irb(main):024:0> j
=> {}
irb(main):025:0> j = Hash.new do |hash, key|
irb(main):026:1* puts "Currently, your hash looks like #{hash}. This does not include the key '#{key}'. We will set it to a default e
mpty array"
irb(main):027:1> []
irb(main):028:1> end
=> {}
irb(main):029:0> j
=> {}
irb(main):030:0> j[:new]
Currently, your hash looks like {}. This does not include the key 'new'. We will set it to a default empty array
=> []
irb(main):031:0> j[:old] = :set
=> :set
irb(main):032:0> j
=> {:old=>:set}
irb(main):033:0> j[:new]
Currently, your hash looks like {:old=>:set}. This does not include the key 'new'. We will set it to a default empty array
=> []
irb(main):034:0> j
=> {:old=>:set}
irb(main):035:0>
```


* Returning just an array
* The SO answer where you return the whole hash `hsh2 = Hash.new { |hash, key| hash[key] = [] }`

# TODO

- [ ] Maybe look into [`default=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default-3D)

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
# create a new hash, use 0 as the default
> zero_hash = Hash.new(0)
=> {}

# look for a key that doesn't exist,
# get back 0 instead of nil
> zero_hash[:not_here]
=> 0
```

### Going Well

The most common way I reach for this is when I am using my hash as a counter.


# TODO

- [ ] Maybe look into [`default=`](https://ruby-doc.org/core-2.7.1/Hash.html#method-i-default-3D)

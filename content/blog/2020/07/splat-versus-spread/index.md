---
title:  "Comparing Ruby's Splat with JavaScript's Spread"
date:   "2020-07-31T06:59:34.781Z"
categories: ["ruby", "javascript"]
---

At work, we have been developing a mentorship and training program to help provide our team members time for [deliberate practice](https://www.calnewport.com/blog/2010/01/06/the-grandmaster-in-the-corner-office-what-the-study-of-chess-experts-teaches-us-about-building-a-remarkable-life/). A recent Ruby discussion led to reviewing the Ruby ["splat"](https://docs.ruby-lang.org/en/2.0.0/syntax/calling_methods_rdoc.html#label-Array+to+Arguments+Conversion) operator and comparing it to [spread](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax) in JavaScript.

In this post, I'll cover the different ways these operators can be used in both Ruby and JavaScript.

## Arrays

### Building up Array

In both Ruby and JavaScript, you can use splat/spread to build up a new array from existing arrays.

#### Ruby

```ruby
# create initial array
> arr1 = [1, 2, 3]
=> [1, 2, 3]

# without the splat operator
> [arr1, 4]
=> [[1, 2, 3], 4]

# with the splat operator
> [*arr1, 4]
=> [1, 2, 3, 4]

# combining multiple array
[*[1, 2, 3], 4, 5, *[6, 7, 8]]
=> [1, 2, 3, 4, 5, 6, 7, 8]
```

#### Idiomatic Ruby

In Ruby, this approach does not seem very common. Instead, it seems more common to use the [`+`](https://ruby-doc.org/core-2.7.0/Array.html#method-i-2B) operator:

```ruby
# create initial arrays
> a1 = [1, 2, 3]
=> [1, 2, 3]
> a2 = [3, 4, 5]
=> [3, 4, 5]

# create a new, combined array
a1 + a2
=> [1, 2, 3, 3, 4, 5]
```

It's probably _even more_ common to see options that update (or mutate) the original array rather than returning a new one, but that is a different use case than what we are covering here.

#### JavaScript

I see this more commonly used in JavaScript, especially in projects that follow functional paradigms. Because this builds up a _new_ array, you are not mutating state. This means you can use this strategy and still define [pure functions](https://en.wikipedia.org/wiki/Pure_function).

```javascript
// create initial array
> let arr1 = [1, 2, 3]
=> (3) [1, 2, 3]

// without the spread operator
> [arr1, 4]
=> (2) [Array(3), 4]

// with the spread operator
> [...arr1, 4]
(4) [1, 2, 3, 4]

// combining multiple array
> [...[1, 2, 3], 4, 5, ...[6, 7, 8]]
=> (8) [1, 2, 3, 4, 5, 6, 7, 8]
```

### Passing into functions

These operators allow functions to take in an unknown number of arguments and will combine them into a single array argument.

#### Ruby

This is how I most commonly see this operator used in Ruby-land. I think this is because it can lend itself well to building [DSLs](https://martinfowler.com/books/dsl.html) that look more like written prose.

```ruby
def greet_friends(*friends)
  "Hello, #{friends.join(', ')}"
end

> greet_friends "joey"
=> "Hello, joey"

> greet_friends %w(joey ross rachel chandler phoebe monica)
=> "Hello, joey, ross, rachel, chandler, phoebe, monica"
```

#### JavaScript

```javascript
const greetClassmates = (...classmates) => (
  `Hello, ${classmates.join(", ")}` 
)

> greetClassmates("abed", "annie", "troy", "jeff", "britta", "pierce", "shirley", "señor chang")
=> "Hello, abed, annie, troy, jeff, britta, pierce, shirley, señor chang"
```

You may also see this referred to as the [rest parameter](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters) syntax.

## Hashes and Objects

For our Ruby examples, we will be using a [`Hash`](https://ruby-doc.org/core-2.7.1/Hash.html). For our JavaScript examples, we will be using an [`Object`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object). Fortunately, the syntax for creating the structures is the same in both languages.

### Two options for Ruby

In Ruby, you can use the [Array version of the splat operator (`*`)](https://docs.ruby-lang.org/en/2.0.0/syntax/calling_methods_rdoc.html#label-Array+to+Arguments+Conversion) for some operations, but there is also a [Hash-specific operator as well (`**`)](https://docs.ruby-lang.org/en/2.0.0/syntax/calling_methods_rdoc.html#label-Hash+to+Keyword+Arguments+Conversion). Here's how they both work:

```ruby
# create our initial hash
> h = { name: "morty smith", age: 14 }

# use `*` operator, or array-version of splat
> ar = *h
=> [[:name, "morty smith"], [:age, 14]]

# turn this back into a Hash with 
# https://www.rubydoc.info/stdlib/core/Kernel:Hash
> Hash[[[:name, "morty smith"], [:age, 14]]]
=> {:name=>"morty smith", :age=>14}

# use the `**` operator, or hash-version of splat
> h2 = {**h}
=> {:name=>"morty smith", :age=>14}
```

We will go through the `**` operator as it behaves more similarly to the spread operator in JavaScript. It's worth noting the behavior of the `*` version as it's similar to what you may encounter when working with the [`Enumberable`](https://ruby-doc.org/core-2.7.1/Enumerable.html) module.

```ruby
> h.map { |element| element }
=> [[:name, "morty smith"], [:age, 14]]
```

### Building up new objects

Just like we saw [above](#building-up-array), we can use these operators to build up new hashes/objects from existing ones.

### Ruby

```ruby
# create our initial hash
> bob = {name: "Bob", age: 46}

# without the splat operator,
# you will get a syntax error
> {bob, business: "Bob's Burgers"}
=> SyntaxError ((irb):3: syntax error, unexpected ',', expecting =>)

# splat it to create a new hash with additional fields
> {**bob, business: "Bob's Burgers"}
=> {:name=>"Bob", :age=>46, :business=>"Bob's Burgers"}
```

#### JavaScript

```JavaScript
// create our initial object
> let richard = {name: "Richard" , age: 31}

// without the spread operator,
// javascript will re-use the variable name as the key
// with a value of the original object
> {richard, business: "Pied Piper"}
=> {richard:  {name: "Richard" , age: 31}, business: "Pied Piper"}

// spread it to create a new object
// with additional fields
> { ...richard, business: "Pied Piper"}
=> {name: "Richard", age: 31, business: "Pied Piper"}
```

### Passing into functions

#### Ruby

```ruby
def greet(name, **options)
  greeting = options[:greeting] || "Hello"

  "#{greeting}, #{name}"
end

greet("teddy")
=> "Hello, teddy"

greet("mort", greeting: "Hiya")
=> "Hiya, mort"
```

In Ruby, you can leave off the `**` and it will default to allowing your last argument to be a hash (or not):

```ruby
def greet_two(name, options)
  greeting = options[:greeting] || "Hello"

  "#{greeting}, #{name}"
end

greet_two("jimmy", greeting: "get out of here")
=> "get out of here, jimmy" 
```

you can also treat the last argument as _not_ a Hash

```ruby
def greet_three(name, greeting)
  "#{greeting}, #{name}"
end

# pass in not a hash
greet_three(
  "Mr. Fischoeder",
  "We'll have the rent next week"
)
=> "We'll have the rent next week, Mr. Fischoeder"

# it can accept a hash,
# but we didn't set it up to handle that very well
> greet_three(
  "Mr. Fischoeder",
  greeting: "We'll have the rent next week"
)
=> "{:greeting=>\"We'll have the rent next week\"}, Mr. Fischoeder"
```

#### JavaScript

JavaScript doesn't support the curly-bracket-less key-value pair syntax as a function argument.

```javascript
const f = (...o) => { console.log(o) }

> f(a: 1, b: 2)
=> VM202:1 Uncaught SyntaxError: missing ) after argument list
```

Instead, you can combine JavaScript's support for [object destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment) with the splat operator. This lets you pull out certain parts of the object and keep a reference to everything else.

```javascript
const fullCast = {
  stars: ["Leading Lady", "Main Man"],
  supporting: ["Nosy Neighbor", "Funny Friend"]
}

// Destructure the object to access `stars`,
// but don't get access anything else
const basicCredits = ({stars: stars}) => {
  console.log(`Starring: ${stars.join(", ")}!`)
}

> basicCredits(fullCast)
=> "Starring: Leading Lady, Main Man!"

// Destructure the object to access `stars`,
// and put the rest of the object in `rest`
const inclusiveCredits = ({stars: stars, ...rest}) => {
  console.log(`Starring: ${stars.join(", ")}!`)
  console.log(`Full cast includes...`, rest)
}

> inclusiveCredits(fullCast)
=> "Starring: Leading Lady, Main Man!"
   "Full cast includes... {supporting: Array(2)}"
  ```

## Conclusion

We've covered how the `splat` and `spread` operators can be used in their respective languages for building up new instances of data structures as well as for function arguments.

Hopefully, this post `spread` some knowledge, and can be useful as a quick reference for how to use these operators in different scenarios.

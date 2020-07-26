---
title:  "Comparing Ruby's Splat with JavaScript's Spread"
date:   "2020-06-24T17:23:34.781Z"
categories: ["ruby", "javascript"]
---

At work, we have been developing a mentorship and training program to help provider our more junior team members time for [deliberate practice](https://www.calnewport.com/blog/2010/01/06/the-grandmaster-in-the-corner-office-what-the-study-of-chess-experts-teaches-us-about-building-a-remarkable-life/). Our team's primary applications are built using Ruby on Rails, but we increasing our usage of React as well. Since we have a polyglot stack, we will often discuss topics by comparing Ruby to JavaScript (Elixir often makes its way into discussions as well 💜). A recent Ruby discussion led to reviewing the Ruby ["splat"](https://docs.ruby-lang.org/en/2.0.0/syntax/calling_methods_rdoc.html#label-Array+to+Arguments+Conversion) (and also [here](https://docs.ruby-lang.org/en/2.0.0/syntax/calling_methods_rdoc.html#label-Hash+to+Keyword+Arguments+Conversion) for hashes) operator and comparing it to [spread](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax) in JavaScript.

In this post, I'll cover the different ways these operators can be use and how to use them in both Ruby and JavaScript.

## Arrays

### Building up Array

In both Ruby and JavaScript you can use splat/spread to build up new array for existing arrays. I see this more commonly used in JavaScript, especially in projects that try leverage functional paradigms. Because this build up a _new_ array, you are not mutating state and can define pure functions.

#### Ruby

```ruby
## create initial array
> arr1 = [1,2,3]
=> [1, 2, 3]

## without the splat opartor
> arr2 = [arr1, 4]
=> [[1, 2, 3], 4]

## with the splat operator
> arr2 = [*arr1, 4]
=> [1, 2, 3, 4]

## combining multiple array
[*[1,2,3], 4, 5, *[6,7,8]]
=> [1, 2, 3, 4, 5, 6, 7, 8]
```

#### JavaScript

```javascript
// create initial array
> arr1 = [1,2,3]
=> (3) [1, 2, 3]

// without the spread operator
> arr2 = [arr1, 4]
=> (2) [Array(3), 4]

// with the spread operator
arr2 = [...arr1, 4]
(4) [1, 2, 3, 4]

// combining multiple array
> [...[1,2,3], 4, 5, ...[6,7,8]]
=> (8) [1, 2, 3, 4, 5, 6, 7, 8]
```

### Passing into functions

Take in an unknown number of arguments and put them all into the same array. This is how I most commonly see this operator used in Ruby-land. I think this is because it can lend itself well to building DSLs that look more like written prose. 

#### Ruby

```ruby
def greet_friends(*friends)
  "Hello, #{friends.join(', ')}"
end

> greet_friends("joey")
=> "Hello, joey"

> greet_friends(%w(joey ross rachel chandler phoebe monica))
=> "Hello, joey, ross, rachel, chandler, phoebe, monica"
```

#### JavaScript

```javascript
const greet_classmates = (...classmates) => (`Hello, ${classmates.join(", ")}` )

> greet_classmates("abed", "annie", "troy", "jeff", "britta", "pierce", "shirley", "señor chang")
=> "Hello, abed, annie, troy, jeff, britta, pierce, shirley, señor chang"
```

## Hashes and Objects

For our Ruby examples, we will be using a [`Hash`](https://ruby-doc.org/core-2.7.1/Hash.html). For our JavaScript examples we will be using an [`Object`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object). Fortunately, the syntax for creating the structures is the same in both languages.

### Two options for Ruby

In Ruby, you can use the Array version of the splat operator (`*`) for some operations, but there is also a Hash-specific operator as well (`**`). Here's how they both work:

```ruby
# create our initial hash
> h = { name: "morty smith", age: 14 }

# use `*` operator, or array-version of splat
> ar = *h
=> [[:name, "morty smith"], [:age, 14]]

# use the `**` operator, or hash-version of splat
> h2 = {**h}
=> {:name=>"morty smith", :age=>14}
```

We will go throug the `**` operator as the behaves more similarly to the spread operator in JavaScript. It's worth noting the behavior of the `*` version as it's similar to what you may encounter when working with the [`Enumberable`](https://ruby-doc.org/core-2.7.1/Enumerable.html) module.

```ruby
> h.map { |element| element }
=> [[:name, "morty smith"], [:age, 14]]
```

### Building up new objects

### Ruby

```ruby
> {**{ name: "Bob", age: 46}, business: "Bob's Burgers"}
=> {:name=>"Bob", :age=>46, :business=>"Bob's Burgers"}
```

#### JavaScript

```JavaScript
> { ...{name: "Richard" , age: 31}, business: "Pied Piper"}
=> {name: "Richard", age: 31, business: "Pied Piper"}
```

#### Passing into functions

### Ruby

Ruby will default to having the last argument of a method take in a Hash. 

---
layout: post
title:  "Memoization in Ruby"
date:   2015-03-08 13:20:02
categories: refactoring
---

Meomization in Ruby is a pretty simple to accomplish.

You basically use an instance variable and the `||=` operator:

```ruby
def memoized_value
  @memoized_value ||= long_running_value_getter
end
```

##How `||=` works

The `||=` operator is equivalent to

```ruby
def memoized_value
  @memoized_value || @memoized_value = long_running_value_getter
end
```

which can be thought of as

```ruby
def memoized_value
  if @memoized_value
    @memoized_value
  else
    @memoized_value = long_running_value_getter
  end
end
```

the `||` and `if` checks work well with Ruby because an uninitialized instance variable will return `nil`

```ruby
>> @variable #=> nil
>> @variable = 'value' #=> "value"
>> @variable #=> "value"
```

 which is falsey

 ```ruby
describe NilClass do
  subject { nil }

  it 'should be false' do
    expect(subject).to be_falsy
  end
end

Finished in 0.00151 seconds (files took 0.11127 seconds to load)
1 example, 0 failures
```

##How it helps

In the above example I am setting the instance variable equal to the value of some fictitious `long_running_value_getter` method. The name of the made up method is intentional. Memoization is useful as a way to cache the value of method that has the potential to take some significant amount of time to run.

###A dramatized example

Let's say that this is the definition of our `long_running_value_getter` method

```ruby
def long_running_value_getter
  sleep 5
  (1..10_000).to_a
end
```

if you only need to use the result of `long_running_value_getter` once you're fine setting it to a variable within the method call and using it but the problem comes in when you are using it multiple times across, multiple methods.

```ruby
class NoMemo
  def call
    evens + odds
  end

  def evens
    long_running_value_getter.select { |v| v % 2 == 0 }
  end

  def odds
    long_running_value_getter.select { |v| v % 2 != 0 }
  end

  private

  def long_running_value_getter
    sleep 5
    (1..10_000).to_a
  end
end
```

this will end up calling the `long_running_value_getter` multiple times.

With memoization we will only need to call the `long_running_value_getter` once.

```ruby
class MoMemo
  def call
    evens + odds
  end

  def evens
    values.select { |v| v % 2 == 0 }
  end

  def odds
    values.select { |v| v % 2 != 0 }
  end

  private

  def values
    @values ||= long_running_value_getter
  end

  def long_running_value_getter
    sleep 5
    (1..10_000).to_a
  end
end
```

Notice that the memoization is done in the `values` method and we now use `values` in both `evens` and `odds` instead of `long_running_value_getter`.

We can take a look at the benchmarks to see the time difference


```ruby
Benchmark.bmbm do |x|
  x.report('No memoization') { NoMemo.new.call }
  x.report('Memoization') { MoMemo.new.call }
end

Rehearsal --------------------------------------------------

No memoization   0.010000   0.000000   0.010000 ( 10.006714)
Memoization      0.000000   0.000000   0.000000 (  5.004335)
----------------------------------------- total: 0.010000sec

                     user     system      total        real
No memoization   0.000000   0.000000   0.000000 ( 10.012453)
Memoization      0.000000   0.000000   0.000000 (  5.001990)
```

The results are pretty obvious. Since, for the effect of having a long running process, we have a `sleep 5` in the `long_running_value_getter` method, the bulk of the time would be related to that five seconds of sleeping. The non-memoized` version had to do this twice and as a result has a runtime of about 10 seconds (2 calls * 5 seconds) while the memoized version only had to do this once so it has a run time of about 5 seconds.

### Potential Pitfalls

You want to make sure when you are working with memoized values you use non-destructive methods (in Ruby these generally have end with a bang `!`), that is, do you not want to change the memozied value.

Let's take a look at the following example.

```ruby
Cat = Struct.new(:type, :sex, :featured) do
  def to_s
    "#{sex}, #{type}"
  end
end

class CatSeller
  def newsletter
    %Q(
      We have some fantastic cats for sale this month!

      <h1>Featured Cats</h1>
      #{featured_cats.map { |cat| "<li>#{cat}</li>" }.join("\n")}

      <h1>All Cats</h1>
      #{cats.join(',')}
    )
  end

  def featured_cats
    cats.select! { |cat| cat.featured }
  end

  def cats
    @cats ||= fetch_cats
  end

  # We will pretend we are getting this from an API or DB
  def fetch_cats
    [
      Cat.new('Lion', 'Male', false),
      Cat.new('Leopard', 'Female', false),
      Cat.new('Snow Leopard', 'Male', true),
      Cat.new('Mountain Lion', 'Male', true),
      Cat.new('Tiger', 'Female', false),
    ]
  end
end
```

We have a cat seller that creates a newsletter to send out to potential buyers. The newsletter headlines featured cats at the tops then lists all available cats.

If we run this we see output like the following.

```html
We have some fantastic cats for sale this month!

<h1>Featured Cats</h1>
<li>Male, Snow Leopard</li>
<li>Male, Mountain Lion</li>

<h1>All Cats</h1>
<li>Male, Snow Leopard</li>
<li>Male, Mountain Lion</li>
```

We end up only seeing the featured cats in both places.

Let's take a look at why this happens.

When we first create our `CatSeller` it seems to have all of its cats
```ruby
>> cat_seller.cats
=> [#<struct Cat type="Lion", sex="Male", featured=false>, #<struct Cat type="Leopard", sex="Female", featured=false>, #<struct Cat type="Snow Leopard", sex="Male", featured=true>, #<struct Cat type="Mountain Lion", sex="Male", featured=true>, #<struct Cat type="Tiger", sex="Female", featured=false>]
```

And the `featured_cats` method seems to work

```ruby
>> cat_seller.featured_cats
=> [#<struct Cat type="Snow Leopard", sex="Male", featured=true>, #<struct Cat type="Mountain Lion", sex="Male", featured=true>]
```

but now when we check on our cats again only the featured ones are left

```ruby
>> cat_seller.cats
=> [#<struct Cat type="Snow Leopard", sex="Male", featured=true>, #<struct Cat type="Mountain Lion", sex="Male", featured=true>]
```

when we look in our `featured_cats` method

```ruby
def featured_cats
  cats.select! { |cat| cat.featured }
end
```

we see it uses [`select!`](http://ruby-doc.org/core-2.2.0/Array.html#method-i-select-21) which **deletes** the elements that are not featured, changing the underlying `cats` array.

Instead we would want to use the non-destructive or "safe" version, `select` without the `!`

```ruby
def featured_cats
  cats.select { |cat| cat.featured }
end
```

now our result keeps our all of our cats around

```html
We have some fantastic cats for sale this month!

<h1>Featured Cats</h1>
Male, Snow Leopard
Male, Mountain Lion

<h1>All Cats</h1>
<li>Male, Lion</li>
<li>Female, Leopard</li>
<li>Male, Snow Leopard</li>
<li>Male, Mountain Lion</li>
<li>Female, Tiger</li>
```


### Reducing Importance of Ordering

Generally when you see reference to [Connascence of Position](http://en.wikipedia.org/wiki/Connascence_%28computer_programming%29#Connascence_of_Position_.28CoP.29) it references the position of arguments in a function or method. Memoization can also reduce the requirements for methods to be called in a certain position. There is probably a properly named concept for this but since I do not know the name we will consider it an example of Connascence of Position.

Without memoization we will either have to call the same method that is either computationally or temporally expensive multiple times

```ruby
class TwitterFeed
  def display
    display_posts
    display_top_posters
  end

  def recent_posts
    # fetch recent posts from the API
  end

  def display_posts
    posts = recent_posts
    # logic to display posts
  end

  def display_top_posters
    posts = recent_posts
    # use post info to find most prolific posters
  end
end
```

 which, like shown in the earlier example will hurt performance, or we will have to make sure we call the method early enough to pass the value around where we need it.

```ruby
class TwitterFeed
  def display
    posts = recent_posts
    display_posts(posts)
    display_top_posters(posts)
  end

  def recent_posts
    # fetch recent posts from the API
  end

  def display_posts(posts)
    # logic to display posts
  end

  def display_top_posters(posts)
    # use post info to find most prolific posters
  end
end
```

The requirement to call a method at a particular time makes the class difficult to use due to lack of flexibility. In the above example if you just wanted to display the posts or the list of top posters you would somehow have to have tweets to pass in which would require to to call and store `recent_posts` and pass in the return value to future calls. The class is set to run in a procedural manner that does not allow for the flexibility of an Object Oriented design.

If you have the same example using memoization

```ruby
class TwitterFeed
  def display
    display_posts
    display_top_posters
  end

  def recent_posts
    # fetch recent posts from the API
    @recent_posts ||= Twitter.tweets
  end

  def display_posts
    # logic to display posts
    recent_posts.each { |post| puts post }
  end

  def display_top_posters
    # use post info to find most prolific posters
    recent_posts.top_posters.each { |poster| puts poster.name }
  end
end
```

We can now call `display_posts` or `display_top_posters` without the need to call `recent_posts` first because they can call `recent_posts` within their method definitions and only incur the cost of fetching the tweets if it is the first time.

## Conclusion

If your class has data that is computationally expensive to process or takes a long time to fetch and this data is used throughout methods in the class, you should consider using memoization. While there are things to be concerned about, such as avoiding manipulating the data directly or keeping around a large chunk of data in memory, I find it to almost always pay off.

If you're unsure of the payoff try out some benchmarks and see!

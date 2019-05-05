---
title:  "Bulk SQL Statements with ActiveRecord - delete_all"
date:   2015-03-28 21:55:02
categories: ['ruby', 'active record', 'optimizations']
---

New Rails developers tend to fall in love with ActiveRecord. More specifically they fall in love with the "magic" of ActiveRecord - the ability to use simple methods to represent some unknown SQL. The problem is, they don't worry about what ActiveRecord is doing under the covers and what the resulting queries actually look like. This can lead to slow methods that require a large amount of memory.

One instance where you may see this is in deleting large sets of data.

Let's say we have an online bookstore with a `Book` model that looks something like this:

```ruby
# == Schema Information
#
# Table name: books
#
#  id         :integer          not null, primary key
#  title      :string(255)
#  isbn       :string(255)
#  author     :string(255)
#  quantity   :integer
#  created_at :datetime
#  updated_at :datetime
#

class Book < ActiveRecord::Base
end
```

We also have an `OrderGenerator` class that uses the `quantity` field to find books we need to re-order from the distributor:

```ruby
class OrderGenerator
  def self.book_list
    Book.where("quantity < 20").pluck(:isbn)
  end
end
```

The problem is, there are books that go out of print and are no longer available for reorder. Our current system does not account for this case and our distributor is getting annoyed that we keep requesting the same out of print books on every order.

We decide to create a cron job that will run regularly to remove any `Book` records that have a `quantity` of zero and have not been restocked in the last month.

We code up an initial solution that looks something like

```ruby
class Book
  def self.remove_with_destroy
    books = where(quantity: 0).where("updated_at < '#{Time.now - 1.month}'")
    books.each do |book|
      book.destroy
    end
  end
end
```

This solution will do what we need but, while simple enough, there are a few issues we should be concerned with.

## Setting Up The Tests

Before I begin I want to discuss how I set up the environment for the benchmarks to come.

Inspired by a great tips in Justin Weiss' book, [*Practicing Rails*](https://www.justinweiss.com/practicing-rails/), I decided that in order to test this properly I should make an actual Rails application based on my example.

Since I work with large, established Rails applications on a daily basis, I tend to forget how easy it is to create a simple, new project for testing a few things out. Between `rails new`, `rails g scaffold`, and `rake db:seed` my testing environment was up in a matter of minutes.

### Creating a large number of Records

To emulate having hundreds of thousands of books I used the `seed.rb` file to create 300,000 `Book` records

```ruby
# db/seeds.rb

(1..300_000).each do |i|
  Book.create(
    author: "Author #{i}",
    title: "Book #{i}",
    isbn: "#{i}",
    quantity: i % 2
  )
end
```

The logic to set the `quantity` field checks if the current index is divisible by 2. This results in half of the books having a quantity of 0 (and the other half 1).

Since I was creating everything at the time of testing I would need to change the `updated_at` field to be older than one month ago to represent the idea we have not been able to restock these book for a long time.

```ruby
Book.where(quantity: 0).limit(50_000).update_all(updated_at: Time.now - 2.months)
```

I chose to update 50,000 records. I felt updating all books with a quantity of 0 breaks the idea that we only want to remove a subset of our out of stock products. I also thought 50,000 records were enough where we should see substantial run times.

I added this to my seeds file as well so I could easily reproduce the entire setup between benchmarks.

Now that I have an environment where I can easily and reliably create a large number of records I can write the method I want to test, seed the database, and benchmark the runtime for each solution.

## destroy vs. delete

As shown before, our current solution is to loop through each book and use the `destroy` method:

```ruby
class Book
  def self.remove_with_destroy
    books = where(quantity: 0).where("updated_at < #{Time.now - 1.month}")
    books.each do |book|
      book.destroy
    end
  end
end
```

### destroy

The [`destroy`](http://api.rubyonrails.org/classes/ActiveRecord/Relation.html#method-i-destroy) method will instantiate an instance of the `ActiveRecord::Model` (the `Book` in this case) in order to allow callbacks to be handled.

The ability to make sure callbacks are fired off when deleting an object can be very useful, especially when dealing with deleting associations or handling any custom `before_destroy` callbacks. However, in our case, we have no assoications or callbacks to be concerned with so this ends up being unncessary overhead.

Another issue with this method is that we end up running separate queries for each deletion. This problem is where the inspiraton for this post came from. While it is hard to know that `destroy` will run callbacks on you objects without looking into the documentation, we can easily watch our logs when running methods to see what qeuries are being made.

This is a tiny subset of the output:

```sql
SQL (0.2ms)  DELETE FROM "books" WHERE "books"."id" = ?  [["id", 1000593]]
(0.5ms)  commit transaction
(0.0ms)  begin transaction
SQL (0.2ms)  DELETE FROM "books" WHERE "books"."id" = ?  [["id", 1000595]]
(0.5ms)  commit transaction
(0.0ms)  begin transaction
SQL (0.2ms)  DELETE FROM "books" WHERE "books"."id" = ?  [["id", 1000598]]
(0.5ms)  commit transaction
(0.0ms)  begin transaction
SQL (0.2ms)  DELETE FROM "books" WHERE "books"."id" = ?  [["id", 1000599]]
(0.5ms)  commit transaction
(0.0ms)  begin transaction
SQL (0.2ms)  DELETE FROM "books" WHERE "books"."id" = ?  [["id", 1000605]]
(0.6ms)  commit transaction
(0.0ms)  begin transaction
```

For each of the 50,000 records we are deleting, we will create a transaction and execute a delete query. While each step is quick, most less than half a millisecond, this will add up when dealing with 50,000 records. We can see the results of a benchmark of a full run below, the total time was almost 100 seconds.

```
                          user     system      total        real
remove_with_destroy   20.090000  21.970000  42.060000 ( 96.173008)
```

### delete

As mentioned before, `destroy` will instantiate an instace of the object about to be deleted in order to run any callbacks. In our case, we are not worried about deleting associated models and we do not have any callbacks, so instantiating an object and running the callbacks ends up requiring more time and memory than necessary.

The documentation for [`destroy`](http://api.rubyonrails.org/classes/ActiveRecord/Relation.html#method-i-destroy) actually addresses this concern **and** suggests an alternative, using the [`delete`](http://api.rubyonrails.org/classes/ActiveRecord/Relation.html#method-i-delete) method:

> The object is instantiated first, therefore all callbacks and filters are fired off before the object is deleted. This method is less efficient than ActiveRecord#delete but allows cleanup methods and other actions to be run.

The delete method no longer instantiates an instance of the `Book` class but rather simply sends a `DELETE` query for the record in question.

To make this change we simply replace our use of `destroy` with `delete`

```ruby
class Book
  def self.remove_with_delete
    books = where(quantity: 0).where("updated_at < #{Time.now - 1.month}")
    books.each do |book|
      book.delete
    end
  end
end
```

When we run the benchmarks now we see a significant improvement

```
user                      system      total        real
remove_with_destroy    12.960000  21.140000  34.100000 ( 49.324933)
```

If we keep an eye on the logs as our method runs we will again see what should be a red flag - we still end up with a query for each record we are deleting.

```sql
SQL (0.5ms)  DELETE FROM "books" WHERE "books"."id" = 1900739
SQL (0.6ms)  DELETE FROM "books" WHERE "books"."id" = 1900740
SQL (0.5ms)  DELETE FROM "books" WHERE "books"."id" = 1900742
SQL (0.6ms)  DELETE FROM "books" WHERE "books"."id" = 1900746
SQL (0.8ms)  DELETE FROM "books" WHERE "books"."id" = 1900748
```

## delete_all

Luckily there is a solution that will stop us from having to instantiate any objects **and** will handle all of the deletion in a single query. That solution is [`delete_all`](http://api.rubyonrails.org/classes/ActiveRecord/Relation.html#method-i-delete_all).


```ruby
class Book
  def self.remove_with_delete_all
    where(quantity: 0).where("updated_at < '#{Time.now - 1.month}'").delete_all
  end
end
```

As promised, this deletes everything in a single query

```sql
SQL (152.4ms)  DELETE FROM "books" WHERE "books"."quantity" = 0 AND (updated_at < '2015-03-04 12:30:09 -0500')
```

The execution time of the query is 152.4ms or 0.1524 seconds. If we look at the results of our benchmark

```
user     system      total        real
remove_with_delete_all   0.060000   0.040000   0.100000 (  0.154572)
```

we noticethat the total exectuion time is only miliseconds more than the query took. This is because there was no additional overhead of creating objects, running callbacks or dealing with associations.

# Conclusion

In conclusion, if you are not worried about callbacks or managing associations `delete_all` will result in speedy deletion of large datasets.

```
                            user     system      total        real
remove_with_destroy     20.090000  21.970000  42.060000 ( 96.173008)
remove_with_destroy     12.960000  21.140000  34.100000 ( 49.324933)
remove_with_delete_all   0.060000   0.040000   0.100000 (  0.154572)
````

More importantly - keep an eye on what your code is doing. As developers we need to own our code and keep it running smoothly. It is difficult to make sure things are running well when we aren't looking for signs that they aren't. When navigating around your site, keep open your server logs or use tools like [MiniProfiler](http://miniprofiler.com/) or [RailsPanel](https://github.com/dejan/rails_panel) for Chrome. When running methods in the console be sure to run with an active query logger running. Only we can prevent poor performing applications!

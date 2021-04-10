---
title:  "Intro to Postgre EXPLAIN"
date:   "2021-05-30T06:30:34.781Z"
categories: ["postgresql", "databases"]
---

For most of the database performance work I've done, simply looking at the queries that are being performed is enough to track down most problems. However, I've always been tempted by the additional data provided by the `EXPLAIN` function. Unfortunately, I didn't really understand the output of these queries and would inevitably go back to other avenues of debugging. Recently, while not under the pressure of debugging any performance issues, I decided to read about `EXPLAIN` queries and how to read query plans. The goal of this post is to provide a quick introduction to the output of `EXPLAIN` to make it easier for you to leverage it.

While other database management systems provide the `EXLPAIN` functionality, I will be focusing on how it works with PostgreSQL since that is what I use most often. This post is essentially me rehashing the [documentation](https://www.postgresql.org/docs/current/using-explain.html
). My goal is to provide enough of an introduction to be able to grok a basic query plan and make reading through the full documentation easier.

## What is EXPLAIN

To start, `EXPLAIN` is a SQL function that, when given a query, will return the plan for how to fetch the data that the database will be using. It does not perform the actual query you into it.

```sql
EXPLAIN(
  SELECT *
  FROM my_table
);

                            QUERY PLAN 
------------------------------------------------------------------
 Seq Scan on my_table  (cost=0.00..10.50 rows=50 width=1572)
(1 row)
```

You can think of the query plan as map directions. It include the steps you will be taking as well as additional information about each step.

![map_directions](./real_directions.png)

Rather than getting information about the distance before you turn left or how long you will be on a highway, you will instead get information about which steps are involved in fetching the data and how many rows of data you will be searching along the way. 


![explain_directions](./explain_directions.png)

## Reading a Query Plan

In our simple example above, there was only one step in our list of direction, sequential scan on `my_table`. For this single step, we get a lot of data about what will be happening. Let's break down was is involved in a row from the query planner.


```sql
Seq Scan on my_table  (cost=0.00..10.50 rows=50 width=1572)
```

### Reading from the Table

Our query plan starts off by letting us know how it's going to read the data from a given databse table.

```sql
Seq Scan on my_table
```

In this example, we will be doing a sequential scan on `my_table`. A sequential scan means each row in the table will be read. There are other types of scans we may see later involving bitmaps or indexes. Check out [this post](https://severalnines.com/database-blog/overview-various-scan-methods-postgresql) for more information on the different types of scans. 

### Cost

The `cost` is made up of two numbers.

```sql
cost=0.00..10.50
```

The first number is the `estimated start-up cost`. This is the amount of time _before_ this step in the plan will run. In our example the start-up cost is `0.00`. This means that this step will run at the beginning of the query execution. This make sense since this is the only step in our plan. 

Let's take a look at a slightly more complex query. In this exapmle, our `where` clause is filtering based on a subquery.

```sql
EXPLAIN(
    SELECT * 
    FROM chat_room_messages
    WHERE author IN (
        SELECT email 
        FROM users 
        WHERE updated_at > '2021-01-01'::date
    )
);
```

This results in a plan with multiple steps.

```sql
                                 QUERY PLAN                                  
-----------------------------------------------------------------------------
 Hash Join  (cost=10.84..22.49 rows=17 width=580)
   Hash Cond: ((chat_room_messages.author)::text = (users.email)::text)
   ->  Seq Scan on chat_room_messages  (cost=0.00..11.30 rows=130 width=580)
   ->  Hash  (cost=10.62..10.62 rows=17 width=516)
         ->  Seq Scan on users  (cost=0.00..10.62 rows=17 width=516)
               Filter: (updated_at > '2021-01-01'::date)
```

As we said before, when the `estimated start-up cost` is `0.00`, that is first steps of the plan. The plan above has **two** rows that seems to have `0.00` for the `estimated start-up cost`:

We have the sequential scan on the `users` table

```sql
->  Seq Scan on users  (cost=0.00..10.62 rows=17 width=516)
```

**and** we also have a sequential scan on the `chat_room_messages` tbale:

```sql
->  Seq Scan on chat_room_messages  (cost=0.00..11.30 rows=130 width=580)
```

Without worrying about the details of what the query plan actually is, the important thing to note is that this is an indication we will start these two steps at the same time and run them in parallel.

While some steps in the plan can run in parallel, we also have a multi-part step that will be run sequentially.

```sql
->  Hash  (cost=10.62..10.62 rows=17 width=516)
        ->  Seq Scan on users  (cost=0.00..10.62 rows=17 width=516)
            Filter: (updated_at > '2021-01-01'::date)
```

This step reveals how nesting is leverages in the query plan output. When reading a query plan, actions will be run starting with the most nested steps. This lines up with what we've learned so far about an `estimated start-up cost` of `0.00` indicating a first step. 

So far, we've only talked about the first number in `cost`, but there are two. The second number is the `estimated total cost`. Together, `cost` gives us an idea of when the step will start and when it will end. 

With these two pieces together, the timelien and `cost` for the above multi-part step should make more sense. We start with the `seq scan on users` since the start-up cost is `0.00`. This step's `estimated total cost` is `10.62`. If we look at the `Hash` line above, we see that its `estimated start-up cost` is `10.62`. This means that as soon as we finish our `seq scan on users`, we will be able to start the `Hash` step. 

### How Much?

So far we have takled about what the two number of cost represent, but not what the values mean. While we've talked about the `cost` as a sort of time-like measurement, it isn't actually a representation of time. The unit type used for `cost` can be changed, but defaults to being based on sequential page fetches. However, the units are arbitrary; what matters is the _relative_ costs. From the [postgreSQL documentation](https://www.postgresql.org/docs/current/runtime-config-query.html#RUNTIME-CONFIG-QUERY-CONSTANTS
)

> The cost variables described in this section are measured on an arbitrary
> scale. Only their relative values matter, hence scaling them all up or down by
> the same factor will result in no change in the planner's choices

So while you may want to track the cost in terms of sequential page fetches or base it on the CPU processing a tuple, what seems to be more important is comparing the numbers relative to other parts of the query plan. 

## Rows

```sql
Seq Scan on my_table  (cost=0.00..10.50 rows=50 width=1572)
```

The next part of a query plan node is `rows`. This is the **estimated** number of rows that will be returned for a given step. Since `EXPLAIN` will not actually run the query it cannot give exact numbers. This estimation can be a useful indication of how much data you're dealing with as well as the potential impact of filtering. 

## Width

The final element of the query plan node that we are going to cover is `width`. The `width` is the average width of the rows in bytes. This is more than just the number of columns, but gives you an idea of the average number of bytes across all columns you will be selecting. Combined with `rows`, you can get a rough idea of the size of the data the database will be scanning.

While it may be obvious that select fewer columns results in less data, `EXPLAIN` can help reveal that. When we perform a `SELECT *` we such a much larger width than if we only `SELECT id`.

```sql
-- SELECT * has a width of 580
# EXPLAIN(SELECT * FROM chat_room_messages);

                              QUERY PLAN                               
-----------------------------------------------------------------------
 Seq Scan on chat_room_messages  (cost=0.00..11.30 rows=130 width=580)
(1 row)

-- SELECT id only 8
# EXPLAIN(SELECT id FROM chat_room_messages);

                             QUERY PLAN                              
---------------------------------------------------------------------
 Seq Scan on chat_room_messages  (cost=0.00..11.30 rows=130 width=8)
(1 row)

```

## Analyze

As previously mentioned, `EXPLAIN` doesn't actually run the query. As a result, it will only give you estimations. This can be useful for gathering the performance characteristics of different potential queries. If you want to get more accurate performance information, you will want to use `EXPLAIN ANALYZE`. With `ANALYZE`, the query will be run. This allows the database to include information about run time, the actual number `row`s, and more.

Let's take a look at one of our previous queries and include an `ANALYZE`.

```sql
EXPLAIN ANALYZE
SELECT * 
FROM chat_room_messages
WHERE author IN (
    SELECT email FROM users WHERE updated_at > '2021-01-01'::date
);

                                                       QUERY PLAN                                                       
------------------------------------------------------------------------------------------------------------------------
 Hash Join  (cost=10.84..22.49 rows=17 width=580) (actual time=0.028..0.033 rows=10 loops=1)
   Hash Cond: ((chat_room_messages.author)::text = (users.email)::text)
   ->  Seq Scan on chat_room_messages  (cost=0.00..11.30 rows=130 width=580) (actual time=0.007..0.008 rows=10 loops=1)
   ->  Hash  (cost=10.62..10.62 rows=17 width=516) (actual time=0.009..0.009 rows=2 loops=1)
         Buckets: 1024  Batches: 1  Memory Usage: 9kB
         ->  Seq Scan on users  (cost=0.00..10.62 rows=17 width=516) (actual time=0.005..0.005 rows=2 loops=1)
               Filter: (updated_at > '2021-01-01'::date)
 Planning Time: 0.104 ms
 Execution Time: 0.050 ms
(9 rows)

```

While the overall plan is the same as before, we now have additional information next to our `cost`, `rows`, `width` tuple. This new tuple gives us our actual runtime statistics.

Now, instead of our arbitrary units for `cost`, we get `time`, which is actual runtime in milliseconds. We also get to see the actual number of `rows` that were returned. We can see some steps where the planner was close (17 versus 10) and others where no quite as much (130 versus 10). These examples are coming from a small, test database which isn't getting regularly `ANALYZE` runs, so I imagine the statistics table is sparse. 

In addition to the additional tuple data, we also have information about our hash function (number buckets, amount of memory used) and overall run times (planning versus actual execution).

I find `ANALYZE` to be an extermly useful addition when using `EXPLAIN`. Since it does require running the query, it will take longer for large queries, so keep that in mind when experimenting. 

## Caveats

The biggest caveat I took away from the documentation is that query plans are specific to the amount of data in the database. As a result, running an `EXPLAIN` locally with a small dataset may not reveal what the query planner is _actually_ going to do on your production system. As an example, with a small enough table, the query planner may prefer a sequential scan over leveraging an index. If you are hoping to confirm your newly added index is going to provide performance gains, you _may_ not see that locally.

From what I can tell, the query plan is more impacted by amount of data (as opposed to taking into acocunt the underlying hardware). As a result, you should be able to get a better idea of production-like characteristics if you load you local databse with enough data (if feasible). However, like many performance tuning tasks, you will ultimately want to confirm on your real data.

## Conclusion

Hopefully, this introduction will empower you to leverage the `EXPLAIN` function. If you find particulary confusing query plans or struggle to know how to leverage the query plan to do something action, remember this quote from the [documentation](https://www.postgresql.org/docs/current/using-explain.html):

> Plan-reading is an art that requires some experience to master



---

- Tree
- How can I highlight part of the query plan?
- Caveats
  - Focused on querying (updates don't show up)
  - Will vary by amount of data; need to test with similar data set 

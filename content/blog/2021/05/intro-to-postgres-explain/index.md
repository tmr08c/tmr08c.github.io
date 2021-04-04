---
title:  "Intro to Postgre EXPLAIN"
date:   "2021-05-30T06:30:34.781Z"
categories: ["postgresql", "databases"]
---

For mow database performance work I've done, simply looking at the queries that are being performed is enough to track down most problems. However, I've always been tempted to run an `EXPLAIN` query to learn more. Unfortunately, I didn't really understand the output of these queries and would inevitably go back to other avenues of debugging. Recently, while not under the pressure of debugging any performance issues, I decided to read about `EXPLAIN` queries and how to read query plans. The goal of this post is to provide a quick introduction and hopefully make it easier for you to read them.

This post is esentially me rehashing the [documentation](https://www.postgresql.org/docs/current/using-explain.html
) which does an excellent job of explaining `EXPLAIN`.While other database management systems provide the `EXLPAIN` functionality, I will be focusing on how it works with PostgreSQL since that is what I use most often. My goal is to provide enough of an introduction to be able to grok a query plan and make reading through the documentation easier.

## What is EXPLAIN

To start, `EXPLAIN` is a SQL function that, when given a query, will return the plan the database plans to use to get the actual data (instead of the actual data you are querying for).

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

You can think of the query plan as map directions. It include the steps you will be taking as well as additional information about each step. Rather than getting information about the distance before you turn left or how long you will be on a highway, you will instead get information about which steps are involved in fetching the data and how many rows of data you will be searching along the way. 


- Tree
- How can I highlight part of the query plan?



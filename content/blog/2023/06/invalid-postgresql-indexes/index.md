---
title: "Invalid PostgreSQL Indexes"
date: 2023-06-30 20:35:02
categories: ["database", "postgresql", "oban"]
---

I want to share a PSA I learned about recently at work. If you use the `CONCURRENTLY` option to rebuild an index in PostgreSQL you will be left with an invalid, partial index if the rebuild fails. From [the docs](https://www.postgresql.org/docs/current/sql-reindex.html):

> If an index build fails with the CONCURRENTLY option, this index is left as “invalid”. Such indexes are useless but it can be convenient to use REINDEX to rebuild them. Note that only REINDEX INDEX is able to perform a concurrent build on an invalid index.

These indexes have the suffix `ccnew` (if there are multiple, they will be numbered). You can use the following query to find your invalid indexes:

```sql
SELECT *
FROM pg_class, pg_index
WHERE pg_index.indisvalid = false
AND pg_index.indexrelid = pg_class.oid;
```

If you run a process to periodically rebuild high-traffic indexes (in our case, we were trying out [Oban.Plugins.Reindexer](https://hexdocs.pm/oban/Oban.Plugins.Reindexer.html)), you could end up with a large number invalid indexes. Worse than the bloated index you were trying to resolve, we found these indexes stuck around through auto vacuums while increasing the time and CPU utilization required to vacuum the table.

We were able to safely remove the invalid indexes and see the performance of our vacuuming improve.

---
title: "EXPLAIN Query with Ecto"
date: 2021-02-28 13:15:02
categories: ["elixir", "ecto", "database"]
---

In a [previous post](/2021/09/intro-to-postgres-explain), I gave a brief introduction to understanding the results of an `EXPLAIN` query (from postgres, at least). In this post, I cover generating an `EXPLAIN`query with [Ecto](https://hexdocs.pm/ecto/Ecto.html).

## Explaining

Fortunately, Ecto provides an explain function, [`Ecto.Adapters.SQL.explain/4`](https://hexdocs.pm/ecto_sql/Ecto.Adapters.SQL.html#explain/4).

The function is provided in the adapters module because not all database management systems support `EXPLAIN` capabilities. `explain/4` will leverage a [DBMS-specific adapter for generating the query](https://hexdocs.pm/ecto_sql/Ecto.Adapters.SQL.Connection.html#c:explain_query/4):

> Executes an EXPLAIN statement or similar for the given query according to its kind and the adapter in the given repository.

The first argument to `Ecto.Adapters.SQL.explain/4` is a `Repo` module. There is also a convenience function, `Repo.explain/3` which alleviates the need to pass in the `Repo`.

```ex
iex()> h Repo.explain/3

     def explain(operation, queryable, opts \\ [])

A convenience function for SQL-based repositories that executes
an EXPLAIN statement or similar depending on the adapter to obtain
statistics for the given query.

See Ecto.Adapters.SQL.explain/4 for more information.
```

## Example

### Set up

To demonstrate, we will create the schemas needed for a blog, `Post` and `Comment`, which are in a one-to-many relationship.

#### Post Schema

```ex
defmodule SampleBlog.Blog.Post do
  use Ecto.Schema
  import Ecto.Changeset

  alias SampleBlog.Blog.Comment

  schema "blog_posts" do
    field :title, :string

    has_many :comments, Comment

    timestamps()
  end

  @doc false
  def changeset(post, attrs) do
    post
    |> cast(attrs, [:title, :inserted_at])
    |> validate_required([:title])
  end
end
```

#### Comment Schema

```ex
defmodule SampleBlog.Blog.Comment do
  use Ecto.Schema
  import Ecto.Changeset

  alias SampleBlog.Blog.Post

  schema "blog_comments" do
    field :body, :string

    belongs_to :post, Post

    timestamps()
  end

  @doc false
  def changeset(comments, attrs) do
    comments
    |> cast(attrs, [:body, :post_id])
    |> validate_required([:body, :post_id])
  end
end
```

### Our Query

The query we want to `EXPLAIN` will be fetching posts with recent activity. Our query will look something like this:

```ex
def posts_with_recent_activity() do
  recent =
    "Etc/UTC"
    |> DateTime.now!()
    |> DateTime.add(-60 * 60 * 24, :second)

  from(p in Post)
  |> join(:left, [p], c in assoc(p, :comments))
  |> preload([p, c], comments: c)
  |> where([p, c], p.inserted_at > ^recent or
                   c.inserted_at > ^recent)
  |> order_by([p], desc: p.inserted_at)
  |> Repo.all()
end
```

In this query, we `join` the `posts` with their `comments`. We also filter both tables based on the `inserted_at` column as our means of finding recent activity. Finally, we order the results by the posts' `inserted_at` column; this simply adds more details to our `EXPLAIN` result

### EXPLAIN versus IO.inspect

A common debugging technique used in Elixir is to insert an `IO.inspect` in-between steps of a pipeline. Unfortunately, this does work for the `explain` function because it expects to receive a queryable (so it will not work at the end of the pipeline) and returns a string instead of what was passed in (so we break the pipeline or lose our results).

If we only care about the `EXPLAIN` results we could remove our call to `Repo.all` and pass our queryable to the `explain` function.

```ex
def posts_with_recent_activity() do
  recent =
    "Etc/UTC"
    |> DateTime.now!()
    |> DateTime.add(-60 * 60 * 24, :second)


  queryable =
    from(p in Post)
    |> join(:left, [p], c in assoc(p, :comments))
    |> preload([p, c], comments: c)
    |> where([p, c], p.inserted_at > ^recent or
                     c.inserted_at > ^recent)
    |> order_by([p], desc: p.inserted_at)
    # Note we no longer call Repo.all

  SampleBlog.Repo
  |> Ecto.Adapters.SQL.explain(:all, queryable)
  |> IO.puts()
end
```

Since the result of `explain` is a string containing the `EXPLAIN` results, I recommend `IO.puts` over `IO.inspect` because it will print the newlines and better format the result.

In practice, making this change can be a bit of a pain. By no longer returning the result of your query, you will break the consumers of this function. This breaking strategy works for a quick peek into what the database is doing. However, if you are tuning the query, you may want to run this multiple times. Returning the query results may be preferable in this case as it will allow you to validate the results continue to be correct.

### Preserving the Pipeline

To preserve the pipeline and avoid copying and pasting my query, I turn to [`Kernel.tap/2`](https://hexdocs.pm/elixir/main/Kernel.html#tap/2). `tap/2` will pipe its first argument as the argument to the function you pass in as the second argument. Rather than passing on the result of the function, `tap/2` returns the original value; this allows us to pipe in the query, `explain` it, and then pass it, unchanged, to our query function. Let's see what this looks like:

```ex
def posts_with_recent_activity() do
  recent =
    "Etc/UTC"
    |> DateTime.now!()
    |> DateTime.add(-60 * 60 * 24, :second)


  from(p in Post)
  |> join(:left, [p], c in assoc(p, :comments))
  |> preload([p, c], comments: c)
  |> where([p, c], p.inserted_at > ^recent or
                   c.inserted_at > ^recent)
  |> order_by([p], desc: p.inserted_at)
  |> tap(fn query ->
    SampleBlog.Repo
    |> Ecto.Adapters.SQL.explain(:all, query)
    |> IO.puts()
  end)
  |> Repo.all()
end
```

Now, we will continue to receive the actual response from our query and have our `EXPLAIN` statement logged as well.

```ex
Blog.posts_with_recent_activity()
|> IO.inspect(label: "Result")
```

```sql
-- explain statement
Sort  (cost=56.17..57.39 rows=489 width=604)
  Sort Key: b0.inserted_at DESC
  ->  Hash Right Join  (cost=13.15..34.32 rows=489 width=604)
        Hash Cond: (b1.post_id = b0.id)
        Filter: ((b0.inserted_at > '2021-12-29 20:54:38'::timestamp without time zone) OR (b1.inserted_at > '2021-12-29 20:54:38'::timestamp without time zone))
        ->  Seq Scan on blog_comments b1  (cost=0.00..18.80 rows=880 width=64)
        ->  Hash  (cost=11.40..11.40 rows=140 width=540)
              ->  Seq Scan on blog_posts b0  (cost=0.00..11.40 rows=140 width=540)
```

```elixir
# IO.inspect
Result: [
  %SampleBlog.Blog.Post{
    __meta__: #Ecto.Schema.Metadata<:loaded, "blog_posts">,
    comments: [
      %SampleBlog.Blog.Comment{
        __meta__: #Ecto.Schema.Metadata<:loaded, "blog_comments">,
        body: "First!",
        id: 31,
        inserted_at: ~N[2021-12-30 20:54:38],
        post: #Ecto.Association.NotLoaded<association :post is not loaded>,
        post_id: 36,
        updated_at: ~N[2021-12-30 20:54:38]
      }
    ],
    id: 36,
    inserted_at: ~N[2021-12-30 20:54:38],
    title: "My first post",
    updated_at: ~N[2021-12-30 20:54:38]
  }
]
```

The [caveats of `EXPLAIN`](/2021/09/intro-to-postgres-explain/#caveats) aside, I have this to be a helpful tool when tuning a query. Thanks to the `tap` function, I can continue to confirm the results of my query are correct while printing out the query plan along the way.

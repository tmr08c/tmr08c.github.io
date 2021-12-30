---
title: "EXPLAIN Query with Ecto"
date: 2021-02-28 13:15:02
categories: ["elixir", "ecto", "database"]
---

In a [previous post](/2021/09/intro-to-postgres-explain), I gave a brief introduction to understanding the results of an `EXPLAIN` query (from postgres, at least). After doing the research to write that post, I have become more comfortable reading `EXPLAIN` queries and find value in ocasionally reaching for them.

My current project is a [Phoenix](https://www.phoenixframework.org/) application with which we are using [Ecto](https://hexdocs.pm/ecto/Ecto.html). In this post, I will cover how to use Ecto to generate an `EXPLAIN` query.

## Explaining

Fortunately, Ecto provides an explain function, [`Ecto.Adapters.SQL.explain/4`](https://hexdocs.pm/ecto_sql/Ecto.Adapters.SQL.html#explain/4). The function is provided in the adapters module because not all database management systems provide `EXPLAIN` capabilites. `explain/4` will leverage the [DBMS-specific adapter for generating the query](https://hexdocs.pm/ecto_sql/Ecto.Adapters.SQL.Connection.html#c:explain_query/4):

> Executes an EXPLAIN statement or similar for the given query according to its kind and the adapter in the given repository.

The first argument to `Ecto.Adapaters.SQL.explain/4` is the `Repo` module.
There is also a `Repo.explain/3` function that is made available as a convenience and alleviates the need to pass in the `Repo`.

```ex
iex()> h Repo.explain/3

                 def explain(operation, queryable, opts \\ [])

A convenience function for SQL-based repositories that executes an EXPLAIN
statement or similar depending on the adapter to obtain statistics for the
given query.

See Ecto.Adapters.SQL.explain/4 for more information.
```

## Example

### Set up

To demonstrate, I create a Phoenix application to represent a blog. It has two schemas, `Post` and `Comment` which are in a one to many relationship.

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

The query we want to `EXPLAIN` will be fetching posts with recent activity. We are defining recent activity as having been created within the last 24 hours. Our query will look something like this:

```ex
def posts_with_recent_activity() do
  recent = DateTime.now!("Etc/UTC") |> DateTime.add(-60 * 60 * 24, :second)

  from(p in Post)
  |> join(:left, [p], c in assoc(p, :comments))
  |> preload([p, c], comments: c)
  |> where([p, c], p.inserted_at > ^recent or c.inserted_at > ^recent)
  |> order_by([p], desc: p.inserted_at)
  |> Repo.all()
end
```

While the specific aren't too important, we are joining the posts with their comments and filtering based on the `inserted_at` field of both tables. To add more to our `EXPLAIN` results, we will also order the table.

Since `EXPLAIN` requires a query in its own right, we cannot simply append it to the end of our pipeline like we could with `IO.inspect`.

If we only care about the `EXPLAIN` results we could remove our call to `Repo.all` and pass our queryable to the `explain` function.

```ex
 def posts_with_recent_activity() do
   recent = DateTime.now!("Etc/UTC") |> DateTime.add(-60 * 60 * 24, :second)

   queryable =
     from(p in Post)
     |> join(:left, [p], c in assoc(p, :comments))
     |> preload([p, c], comments: c)
     |> where([p, c], p.inserted_at > ^recent or c.inserted_at > ^recent)
     |> order_by([p], desc: p.inserted_at)

   Ecto.Adapters.SQL.explain(SampleBlog.Repo, :all, queryable)
   |> IO.puts()
 end
```

Because the result of `explain` is a string containing the `EXPLAIN` results, I `IO.puts` it. I reccommend `IO.puts` over `IO.inspect` because it will print the newlines and better format the result.

In practice, making this change can be a bit of a pain. By no longer retruning the result of your query, you often break downstream work. If you are trying to `EXPLAIN` something that results from particular UI interactions, you may want the query to actually execute to avoid breaking things too much.

To preserve the pipeline and avoid copying and pasting my query, I turn to [`Kernel.tap/2`](https://hexdocs.pm/elixir/main/Kernel.html#tap/2). Released in `1.12`, `tap/2` will pipe its first argument as the argument to the function you pass in as the second argument. Unlike [`Kernel.then/2`](https://hexdocs.pm/elixir/main/Kernel.html#then/2), `tap/2` ignores the result of your function and passes on the value unchanged. For our use, this allows us to pass in our query, explain it, and then pass it, unchanged, to our query function. Let's see what this actually looks like:

```ex
def posts_with_recent_activity() do
  recent = DateTime.now!("Etc/UTC") |> DateTime.add(-60 * 60 * 24, :second)

  from(p in Post)
  |> join(:left, [p], c in assoc(p, :comments))
  |> preload([p, c], comments: c)
  |> where([p, c], p.inserted_at > ^recent or c.inserted_at > ^recent)
  |> order_by([p], desc: p.inserted_at)
  |> tap(fn query -> Ecto.Adapters.SQL.explain(Repo, :all, query) |> IO.puts() end)
  |> Repo.all()
end
```

Now, we will continue to receive the actual response from our query and have our `EXPLAIN` statement logged as well.

```ex
Blog.posts_with_recent_activity()
|> IO.inspect(label: "Result")
```

```
# explain statement
Sort  (cost=56.17..57.39 rows=489 width=604)
  Sort Key: b0.inserted_at DESC
  ->  Hash Right Join  (cost=13.15..34.32 rows=489 width=604)
        Hash Cond: (b1.post_id = b0.id)
        Filter: ((b0.inserted_at > '2021-12-29 20:54:38'::timestamp without time zone) OR (b1.inserted_at > '2021-12-29 20:54:38'::timestamp without time zone))
        ->  Seq Scan on blog_comments b1  (cost=0.00..18.80 rows=880 width=64)
        ->  Hash  (cost=11.40..11.40 rows=140 width=540)
              ->  Seq Scan on blog_posts b0  (cost=0.00..11.40 rows=140 width=540)

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

The [caveats of `EXPLAIN` aside](/2021/09/intro-to-postgres-explain/#caveats), I have found this workflow useful when tuning a query. I can continue to confirm the results of my query are correct while printing out the query plan along the way.

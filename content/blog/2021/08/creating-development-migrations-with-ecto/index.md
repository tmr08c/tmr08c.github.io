---
title: "Creating Development-Specific Migrations with Ecto"
date: "2021-08-30T06:30:34.781Z"
categories: ["elixir", "ecto"]
---

I have recently begun working on a recently created Phoenix application. As a part of my onboarding, I saw an opportunity to simplify the process of getting a development environment setup. Inspired by the `setup` script used by my previous team and GitHub's [Script To Rule Them All](https://github.com/github/scripts-to-rule-them-all) repository, I set out to create a [setup](https://github.com/github/scripts-to-rule-them-all#scriptsetup) script for this new application.

While most of the setup process is straightforward, there was a small issue I was unsure how to solve. Our application relies on a [PostgreSQL Extension](https://www.postgresql.org/docs/current/external-extensions.html). While it's relatively easy to add an extension to PostgreSQL (see [`CREATE EXTENSION`](https://www.postgresql.org/docs/current/sql-createextension.html)), doing this in an [`Ecto.Migration`](https://hexdocs.pm/ecto_sql/Ecto.Migration.html) wouldn't work in production. In following the [principle of least privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege), the postgres role/user used in deployment does not have the superuser access required to run the `CREATE EXTENSION` query.

Due to the inability to run `CREATE EXTENSION` via a migration on production, we started out by simply including the queries to run in the setup section of our project's `README`. While this worked, the worfklow was a bit clunky becuase you needed to `ecto.create` the databse and then maually run the queries to add the extensions before you would be able to run all of your other migrations. Initially, I worked to replicate this workflow in our new `setup` script, using the `--command` flag of the `psql` tool to run our specified queries.

```bash
psql --command="CREATE EXTENSION IF NOT EXIST my_extension"
```

While we eventually got something that worked, it felt a bit brittle. It also didn't work with the default `mix ecto.setup` alias that you get as a part of a [generating a new Phoenix application](https://github.com/phoenixframework/phoenix/blob/e221f88083779a4055bddf3d268f5d23f474bea9/installer/templates/phx_single/mix.exs#L67). While our `bin/setup` script could have alleviated the need for some of our `mix` aliases, it would have been preferable to allow the team to use both. As I was about to call it "good enough for now," I found [this article](https://dashbit.co/blog/automatic-and-manual-ecto-migrations) on the Dashbit blog, titled _Automatic and Manual Migrations_. The post describes scenarios in which you may want to leverage Ecto's migrations, but not want them to automatically be run as a part of your deployment. What this post revealed was the `--migrations-path` flag for `mix ecto.migrate`. The `--migrations-path` option enables you to look in a different directly for your migrations (and thanks to [ecto_sql` 3.4](https://github.com/elixir-ecto/ecto_sql/blob/master/CHANGELOG.md#v340-2020-03-24), we can specify multiple paths).

Having the ability to specify additional paths for migrations meant that, in production, we can limit our migrations to the default `priv/repo/migrations` path, but in development and testing we can add additional paths to check.

Using this concept as the model, I created a new directory, `setup_migrations` within `priv/repo` and generated a migration that would set up the extensions we needed.

```
mix ecto.gen.migration --migrations-path=priv/repo/setup_migrations create_necessary_extension
```

Within the migration itself, we were able to use the same `CREATE EXTENTION IF NOT EXIST` syntax.

```elixir
defmodule MyAp.Repo.Migrations.CreateNecessaryExtension do
  use Ecto.Migration

  def up do
    execute("CREATE EXTENSION IF NOT EXISTS my_extension")
  end

  def down do
    execute("DROP EXTENSION IF EXISTS my_extension")
  end
end
```

To run these migrations, I copied what [Wojtek Mach](https://github.com/wojtekmach) outlines in the blogpost, I added a `migrate_all` alias to our `mix.exs` aliases and used it in our `setup` and `test` aliases:

```elixir
# mix.exs

defp aliases
  [
    "ecto.migrate_all": [
        "ecto.migrate \
            --migrations-path=priv/repo/setup_migrations \
            --migrations-path=priv/repo/migrations"
    ],
    "ecto.setup": [
      "ecto.create",
      "ecto.migrate_all",
      "run priv/repo/seeds.exs"
    ],
    test: [
      "ecto.create --quiet",
      "ecto.migrate_all --quiet",
      "test"
    ]
  ]
end
```

Having our `setup` process run `migrate_all` should cover our bases until we add another "setup" type of migration. If that day comes, my plan is to include a note in the PR that we will need to run `ecto.migrate_all` in our local environments. Since I don't expect this to be a common occurrence, I think this manual intervention is fine. However, if we find it happens more often, we will want to investigate alternatives (e.g., alias `migrate` to `ecto.migrate_all` or creating a `migrate` script to match our [other dev scripts](https://github.com/github/scripts-to-rule-them-all)).

I am optimistic about this solution. I think this achieves my goal of easy project setup, and has little cost on long term project maintenance. That said, I would love to know if there are alternative solutions out there that people are using. Let me know what your team does to provide an easy set up (and environment maintenance) experience for everyone.

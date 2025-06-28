---
title: "Creating Development-Specific Migrations with Ecto"
date: "2021-07-31T18:30:34.781Z"
categories: ["elixir", "ecto", "phoenix", "PETAL"]
---

Inspired by the `setup` script used by my previous team and GitHub's [Scripts To Rule Them All](https://github.com/github/scripts-to-rule-them-all) repository, I recently set out to create a [`setup`](https://github.com/github/scripts-to-rule-them-all#scriptsetup) script (and others) for a greenfield [PETAL stack](https://changelog.com/posts/petal-the-end-to-end-web-stack) application.

When creating the scripts, I hit an issue automating the process of adding [PostgreSQL Extensions](https://www.postgresql.org/docs/current/external-extensions.html). While it's relatively easy to add an extension to PostgreSQL (see [`CREATE EXTENSION`](https://www.postgresql.org/docs/current/sql-createextension.html)), doing this in an [`Ecto.Migration`](https://hexdocs.pm/ecto_sql/Ecto.Migration.html) wouldn't work for us. In following the [principle of least privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege), the Postgres role/user used in deployment does not have the access required to run the `CREATE EXTENSION` query; this means creating extensions in a migration would not work when running in our production environment.

I started my `setup` script journey by replicating the manual extension creation workflow. To do this, I used the `--command` flag with the [`psql`](https://www.postgresql.org/docs/current/app-psql.html) command-line tool to run our specified queries.

```bash
# script/setup
mix ecto.create
psql --command="CREATE EXTENSION IF NOT EXIST my_extension"
mix ecto.migrate
```

While we eventually got something that worked, it was brittle. It required using our new scripts and didn't work well with other `mix ecto` aliases.

As I was about to call it "good enough for now," I found the post, [_Automatic and Manual Migrations_](https://dashbit.co/blog/automatic-and-manual-ecto-migrations) on the Dashbit blog. The post describes scenarios in which you may want to leverage Ecto's migrations but not have them automatically run as a part of your deployment. They accomplished this using the `--migrations-path` flag for `mix ecto.migrate`.

The `--migrations-path` option enables you to look in a different directory for migration files. Thanks to [`ecto_sql` 3.4](https://github.com/elixir-ecto/ecto_sql/blob/master/CHANGELOG.md#v340-2020-03-24), this flag can be included multiple times to specify _multiple paths_ to search. Furthermore, we can utilize the flag differently in different environments. In production, we can limit our migrations to the default `priv/repo/migrations` path, and, in development and testing, we can look in an additional location. In the post, this enabled automatically running "data migration" migrations in development and testing while providing the ability to run them manually in production. We could follow this same pattern to have migrations that "set up" our database in development and testing and ignore them in production.

Using this concept as the model, I generated a migration that would set up the extensions we needed and used the `--migrations-path` to specify an alternative location to store the new migration file (I chose `priv/repo/setup_migrations`).

```bash
mix ecto.gen.migration \
  --migrations-path=priv/repo/setup_migrations \
  create_necessary_extension
```

Within the migration itself, I used the same `CREATE EXTENTION IF NOT EXIST` syntax.

```elixir
defmodule MyApp.Repo.Migrations.CreateNecessaryExtension do
  use Ecto.Migration

  def up do
    execute("CREATE EXTENSION IF NOT EXISTS my_extension")
  end

  def down do
    execute("DROP EXTENSION IF EXISTS my_extension")
  end
end
```

_Note: because we had migrations that relied on the extension existing, I also had to update the generated migration to have a timestamp that was earlier than the dependent migrations._

To run these migrations I copied the blog post and added a `migrate_all` alias to our `mix.exs` aliases list. I then updated our `setup` and `test` aliases to use this new `migrate_all` action.

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

Since our `setup` script was previously using `ecto.create` and `ecto.migrate`, I updated it to use the updated `ecto.setup` alias instead.

```{diff}
# script/setup

- mix ecto.create
- psql --command="CREATE EXTENSION IF NOT EXIST my_extension"
- mix ecto.migrate
+ mix ecto.setup
```

Since it will be uncommon to add setup migrations, one concern I have is forgetting to use `mix ecto.migrate_all` over the default `mix ecto.migrate`. If we do not use the `migrate_all` alias, our development environments will miss out on any new "set up" migrations.

A low-cost solution may be to communicate the need to run `mix ecto.migrate_all` to the team; this could be a note in the PR, a mention during standup, or a note in chat. While not ideal, I expect this will be rare enough to be sustainable.

Alternatively, if you are following GitHub's [Scripts To Rule Them All](https://github.com/github/scripts-to-rule-them-all) pattern (or something similar), you may have another option - you could update the script(s) you have to run migrations to use `mix ecto.migrate_all`. Following GitHub's pattern, [`script/update`](https://github.com/github/scripts-to-rule-them-all#scriptupdate) is the suggested place to run migrations after pulling down new code. If your team already has a similar script, you should be able to transparently change from `mix ecto.migrate` to `mix ecto.migrate_all`.

```{diff}
# script/update

script/bootstrap

echo "==> Updating db..."
- mix ecto.migrate
+ mix ecto.migrate_all
```

Overall, I am optimistic about this solution. I think this achieves my goal of easy project setup while having low long-term project maintenance requirements.

How does your team provide an easy setup (and environment maintenance) experience for everyone? While I am happy with the solution so far, we haven't onboarded enough people to claim success. Please reach out and share alternative options that would improve our new process.

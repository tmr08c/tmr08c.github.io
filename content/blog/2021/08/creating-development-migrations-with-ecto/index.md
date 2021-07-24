---
title: "Creating Development-Specific Migrations with Ecto"
date: "2021-08-30T06:30:34.781Z"
categories: ["elixir", "ecto"]
---

I have recently started on a team that is only a few months into building a new Elixir application. As a part of my onboarding, I saw an opportunity to simplify the process of getting a development environment setup. Inspired by the `setup` script used by my previous team and GitHub's [Script To Rule Them All](https://github.com/github/scripts-to-rule-them-all) repository, I set out to create a [`setup`](https://github.com/github/scripts-to-rule-them-all#scriptsetup) script (and others) for this new application.

While creating the scripts, I hit an issue automating the set up of a [PostgreSQL Extension](https://www.postgresql.org/docs/current/external-extensions.html). While it's relatively easy to add an extension to PostgreSQL (see [`CREATE EXTENSION`](https://www.postgresql.org/docs/current/sql-createextension.html)), doing this in an [`Ecto.Migration`](https://hexdocs.pm/ecto_sql/Ecto.Migration.html) wouldn't work for us in production. In following the [principle of least privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege), the postgres role/user used in deployment does not have the access required to run the `CREATE EXTENSION` query; this means using a migration to create the migration would cause our deploys to fail.

I started my setup script journey by trying to replicate the manual extension creation workflow using the `--command` flag of the `psql` tool to run our specified queries.

```bash
# script/setup
mix ecto.create
psql --command="CREATE EXTENSION IF NOT EXIST my_extension"
mix ecto.migrate
```

While we eventually got something that worked, it was brittle. It required working with out new scripts and didn't work well with the [default `mix ecto.setup` alias](https://github.com/phoenixframework/phoenix/blob/e221f88083779a4055bddf3d268f5d23f474bea9/installer/templates/phx_single/mix.exs#L67) that you get as a part of a generating a new Phoenix application. This required breaking from conventions which wasn't ideal.

As I was about to call it "good enough for now," I found [this article](https://dashbit.co/blog/automatic-and-manual-ecto-migrations) on the Dashbit blog, titled _Automatic and Manual Migrations_. The post describes scenarios in which you may want to leverage Ecto's migrations, but not have them automatically run as a part of your deployment. The way they accomplished this was the `--migrations-path` flag for `mix ecto.migrate`. The `--migrations-path` option enables you to look in a different directory for your migration. Thanks to [`ecto_sql` 3.4](https://github.com/elixir-ecto/ecto_sql/blob/master/CHANGELOG.md#v340-2020-03-24) we can now use this flag to specify _multiple_ paths to search. Having the ability to specify additional paths for migrations meant that, in production, we can limit our migrations to the default `priv/repo/migrations` path, but in development and testing we can look in an additional location (maybe even one that has a migration for creating extensions).

Using this concept as the model, I generated a migration that would set up the extensions we needed, but specified a new path (I chose `priv/repo/setup_migrations`).

```bash
mix ecto.gen.migration \
  --migrations-path=priv/repo/setup_migrations \
  create_necessary_extension
```

Within the migration itself, I was able to use the same `CREATE EXTENTION IF NOT EXIST` syntax.

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

_Note: because we had migrations that relied on the extension existing, I also had to update the generated migration to have a timestamp that came before the dependent migrations._

To run these migrations, I copied the paradigm set out in the blog post and added a `migrate_all` alias to our `mix.exs` aliases. I then updated our `setup` and `test` aliases to use this new `migrate_all` action.

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

Since our `setup` script was already using `ecto.setup`, updating the alias to use `ecto.migrate_all` meant that it would now Just Work&trade;!

I am optimistic about this solution. I think this achieves my goal of easy project setup, and has little cost on long term project maintenance.

One potential pain point with this process is how infrequently it will be leveraged by the team. Since it will be uncommon to add setup migrations, I don't expect the team to make a habit of using `mix ecto.migrate_all` over the default `mix ecto.migrate`. This means that when we _do_ add a new setup migration, we may forget to run it in our local environments.

A low cost solution would be to communicate the need to run `mix ecto.migrate_all` to the team; this could be a note in the PR, a mention during standup, or a note in chat. While not ideal, I expect this will be rare enough to be sustainable.

If you are following GitHub's [Script To Rule Them All](https://github.com/github/scripts-to-rule-them-all) (or something similar), another option could be to update the script(s) you have to run migrations to use `mix ecto.migrate_all`. Following GitHub's pattern, [`script/update`](https://github.com/github/scripts-to-rule-them-all#scriptupdate) is the suggested place to run migrations after pulling down new code. If your team already has a similar script, you should be able to transparently change from `mix ecto.migrate` to `mix ecto.migrate_all`.

```{diff}
# script/update

script/bootstrap

echo "==> Updating db..."
- mix ecto.migrate
+ mix ecto.migrate_all
```

How does your team provide an easy set up (and environment maintenance) experience for everyone? While I am happy with the solution so far, we haven't onboarded enough people to see how it holds up. I would love to know if there are alternative options out there to help improve our new process.

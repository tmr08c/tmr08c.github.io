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

While we eventually got something that worked, it felt a bit brittle. It also didn't work with the default `mix ecto.setup` alias that you get as a part of a [generating a new Phoenix application](https://github.com/phoenixframework/phoenix/blob/e221f88083779a4055bddf3d268f5d23f474bea9/installer/templates/phx_single/mix.exs#L67).

- https://dashbit.co/blog/automatic-and-manual-ecto-migrations

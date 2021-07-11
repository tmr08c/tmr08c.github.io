---
title: "Creating Development-Specific Migrations with Ecto"
date: "2021-08-30T06:30:34.781Z"
categories: ["elixir", "ecto"]
---

I have recently begun working on a recently created Phoenix application. As a part of my onboarding, I saw an opportunity to simplify the process of getting a development environment setup. Inspired by the `setup` script used by my previous team and GitHub's [Script To Rule Them All](https://github.com/github/scripts-to-rule-them-all) repository, I set out to create a [setup](https://github.com/github/scripts-to-rule-them-all#scriptsetup) script for this new application.

While most of the setup process is straightforward, there was a small issue I was unsure how to solve. Our application relies on a [PostgreSQL Extension](https://www.postgresql.org/docs/current/external-extensions.html). While it's relatively easy to add an extension to PostgreSQL (see [`CREATE EXTENSION`](https://www.postgresql.org/docs/current/sql-createextension.html), doing this in an [`Ecto.Migration`](https://hexdocs.pm/ecto_sql/Ecto.Migration.html) wouldn't work in production.

- extension needs super user
- https://en.wikipedia.org/wiki/Principle_of_least_privilege

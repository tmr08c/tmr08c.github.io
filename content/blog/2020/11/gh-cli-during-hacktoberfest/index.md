---
title: 'Leveraging the GitHub CLI During Hacktoberfest'
date: '2020-11-04T05:31:13.265Z'
categories: ['open source']
---

Recently [coming out of beta](https://github.blog/2020-09-17-github-cli-1-0-is-now-available/), the [GitHub CLI](https://github.com/cli/cli) is a tool for interacting with GitHub directly from your terminal. A successor to [hub](https://github.com/github/hub), CLI provides commands for interacting with most of GitHub:

```bash
CORE COMMANDS
  gist:       Manage gists
  issue:      Manage issues
  pr:         Manage pull requests
  release:    Manage GitHub releases
  repo:       Create, clone, fork, and view repositories
```

During this year's [Hacktoberfest](https://hacktoberfest.digitalocean.com/), I found out how the tool excels in helping make contributions to open source projects.

## Forking

With CLI, you can fork a repository directly from your terminal.

```bash
› gh repo fork cli/cli
- Forking cli/cli...
✓ Created fork tmr08c/cli
? Would you like to clone the fork? Yes
Cloning into 'cli'..
```

 It will also offer to clone the repository for you, which sets your `upstream` and `origin` remotes to the original repository and your fork, respectively.

```bash
› git remote -v
origin	git@github.com:tmr08c/cli.git (fetch)
origin	git@github.com:tmr08c/cli.git (push)
upstream	git@github.com:cli/cli.git (fetch)
upstream	git@github.com:cli/cli.git (push)
```

Not only does this make it easy to get access to a repository's code, but it also sets you up to successfully contribute to the project.

## Making Pull Requests

With CLI, your workflow is the same whether you open a Pull Request against a project you own or one you contribute to; you use `gh pr create`.

```bash
› gh pr create

Creating pull request for tmr08c:my-fake-pr-branch
  into trunk in cli/cli
```

When contributing to an open source project, this will create a Pull Request in the original project and point it to your fork's branch.

## Conclusion

While fun and inspiring, contributing to open source can be scary at times; CLI helps reduce some of the anxiety around open source by providing an easy to use tool that sets you up for open source success by following best practices.

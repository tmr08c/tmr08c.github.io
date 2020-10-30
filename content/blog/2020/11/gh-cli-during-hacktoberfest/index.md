---
title: 'Leveraging the GitHu CLI During Hacktoberfest'
date: '2020-11-01T20:17:13.265Z'
categories: ['open source']
---

[Now 1.0](https://github.blog/2020-09-17-github-cli-1-0-is-now-available/), the [GitHub CLI](https://github.com/cli/cli) is a tool for interacting with GitHub directly from your terminal. A successor to [hub](https://github.com/github/hub), CLI (as it' referred to) provides commands for interacting with most of GitHub:

```bash
CORE COMMANDS
  gist:       Manage gists
  issue:      Manage issues
  pr:         Manage pull requests
  release:    Manage GitHub releases
  repo:       Create, clone, fork, and view repositories
```

For daily work, I have been encorporating CLI into my workflow for tasks such as creating Pull Requests (only scractching the surface of what I _can_ be doing). During this year's [Hacktoberfest](https://hacktoberfest.digitalocean.com/) I saw how the tool can really shine when making open source contributions. 

## Forking

With CLI, you can fork a repository directly from your terminal. It will also offer to clone the repository for you and reproperly set up your `upstream` and `origin` remotes:

```bash
› gh repo fork cli/cli   
- Forking cli/cli...
✓ Created fork tmr08c/cli
? Would you like to clone the fork? Yes
Cloning into 'cli'..

#...

› git remote -v 
origin	git@github.com:tmr08c/cli.git (fetch)
origin	git@github.com:tmr08c/cli.git (push)
upstream	git@github.com:cli/cli.git (fetch)
upstream	git@github.com:cli/cli.git (push)
```

When exploring different Hacktoberfest-labeled projects, the ability to quickly pull down a copy of the code to work on was a termendous help. 

## Making Pull Requests 

When I was ready to make a Pull Request, CLI was set up to default to opening the Pull Request on the upstream repository:

```bash
› gh pr create

Creating pull request for tmr08c:my-fake-pr-branch into main in rubyforgood/casa
```

## Conclusion

While there are all things that could be done easily manually and through the GitHub UI, the CLI provides a slick tool for interacting with GitHub and the wider open source world. While fun and inspiring, contributing to open source can be scary at times. CLI helps reduce some of the anxity around open source by providing an easy to use tool that sets you up for open source success by following best practices.

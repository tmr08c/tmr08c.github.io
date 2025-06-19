---
date: '2020-12-04T06:01:13'
tags:
- open source
title: Leveraging the GitHub CLI During Hacktoberfest - Maintaining
---

In a [previous post](/2020/11/gh-cli-for-contributing), I mentioned using the [GitHub CLI](https://github.com/cli/cli) during Hacktoberfest to make it easier to contribute to projects. This year, I was also fortunate to be able to participate in Hacktoberfest as a collaborator on the [`devise-security`](https://github.com/devise-security/devise-security/issues/232) project.

The command I used the most as a collaborator was `gh pr checkout` for checking out the code related to a Pull Request. Using this command is now my go-to way of getting access to the code I am reviewing.

As you may be able able to guess from the name, the `pr checkout` command will, similar to [`git-checkout`](https://git-scm.com/docs/git-checkout) or [`git-switch`](https://git-scm.com/docs/git-switch), check out the code for the Pull Request it is given as an argument. 

```bash
> gh pr checkout --help

Check out a pull request in git

USAGE
  gh pr checkout {<number> | <url> | <branch>} [flags]
```

The main reason I find myself using `pr checkout` over regular `git-checkout` is the fact that it will `fetch` and `pull` the latest version of the branch from your remote when switching to the branch. At a minimum, this saves some typing, but, for me, this more often prevents me from forgetting to pull in the latest changes and reviewing out-of-date code.

When working on an open source project, I find CLI's `pr checkout` command to be even more powerful, as it will seamlessly check out the `ref` for the Pull Request that is [created by GitHub](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/checking-out-pull-requests-locally#modifying-an-inactive-pull-request-locally):

> Once a pull request is opened, GitHub stores all of the changes remotely. In other words, commits in a pull request are available in a repository even before the pull request is merged. You can fetch an open pull request and recreate it as your own.

Below, you can see how the command `fetch`es the latest changes from the remotes. It will then switch to a reference that is based on the `master` branch of someone else's `fork` of the project.

```bash {hl_lines=[7,8]}
› gh pr checkout 245
remote: Enumerating objects: 7, done.
remote: Counting objects: 100% (7/7), done.
remote: Total 12 (delta 7), reused 7 (delta 7), pack-reused 5
Unpacking objects: 100% (12/12), 1.40 KiB | 75.00 KiB/s, done.
From https://github.com/devise-security/devise-security
 * [new ref]         refs/pull/245/head -> DevS1993/master
Switched to branch 'DevS1993/master'
```

The [alternative workflow](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/checking-out-pull-requests-locally#modifying-an-inactive-pull-request-locally) would be something like:

```bash
git fetch origin pull/245/head:DevS1993/master
git switch DevS1993/master
```

While the alternative method is only two commands, `pr checkout` is less typing, and it provides more user-friendly ergonomics. Not only does it save you from having to think of a new, temporary branch name, but it also handles actually `switch`ing to that branch as well. CLI knows you want to check out this code and makes it easy to do just that.

Additionally, with CLI, you can use the same command to checkout a Pull Request for the first time and the N'th time; you don't have to recall if you have already created a branch for the `ref` or not and you will always get the most up-to-date version of the code.

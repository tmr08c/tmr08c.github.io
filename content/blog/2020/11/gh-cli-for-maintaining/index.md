---
title: 'Leveraging the GitHub CLI During Hacktoberfest - Maintaining'
date: '2020-11-08T05:31:13.265Z'
categories: ['open source']
---

In a [previous post](/2020/11/gh-cli-during-hacktoberfest), I mentioned using the [GitHub CLI](https://github.com/cli/cli) during Hacktoberfest to make it easier to contribute to projects. This year, I was fortunate to also [participate in Hacktoberfest](https://github.com/devise-security/devise-security/issues/232) as a collaborator on the [`devise-security`](https://github.com/devise-security/devise-security) project.

When focusing on collaborating, CLI was still incredibly useful. The command I used most was `gh pr checkout` for checking out the code related to a Pull Request.

As you may be able able to guess from the name, the `pr checkout` command will, similar to [`git-checkout`](https://git-scm.com/docs/git-checkout) or [`git-switch`](https://git-scm.com/docs/git-switch)), will check out the code for the Pull Request it is given as an argument. The Pull Request can be given as a number, the URL, or the branch name.

```bash
Check out a pull request in git

USAGE
  gh pr checkout {<number> | <url> | <branch>} [flags]
```

This is my go-to way for checking out code in a Pull Request that I am code reviewing.

The main reason I find myself using `pr checkout` over regular `git-checkout` is the fact that it will `fetch` and `pull` the latest version of the branch from your remote when switching to the branch. At a minimum, this saves some typing, but, for me, this more often saves me from the trouble of forgetting to pull in the latest changes and not having the most up-to-date code.

When working on an open source project, I find CLI's `pr checkout` command is even more powerful as it will seamlessly check out the `ref` for the Pull Request that is [created by GitHub](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/checking-out-pull-requests-locally#modifying-an-inactive-pull-request-locally):

> Once a pull request is opened, GitHub stores all of the changes remotely. In other words, commits in a pull request are available in a repository even before the pull request is merged. You can fetch an open pull request and recreate it as your own.

Below, you can see how the command `fetch`es the latest changes from my remotes and then switches to a reference that the `master` branch of someone else's `fork` of the project.

```bash{7-8}
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
git fetch origin pull/ID/head:BRANCHNAME
git switch BRANCHNAME
```

While only two commands, it's still twice as many using CLI! More importantly for me is the ergonomics of CLI versus manually fetching the ref. 


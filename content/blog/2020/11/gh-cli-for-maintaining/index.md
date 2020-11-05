---
title: 'Leveraging the GitHub CLI During Hacktoberfest - Maintaining'
date: '2020-11-08T05:31:13.265Z'
categories: ['open source']
---

In a [previous post](/2020/11/gh-cli-during-hacktoberfest), I mentioned using the [GitHub CLI](https://github.com/cli/cli) during Hacktoberfest to make it easier to contribute to projects. This year, I was also fortunate to also [participate in Hacktoberfest](https://github.com/devise-security/devise-security/issues/232) as a collaboartor on the [`devise-security`](https://github.com/devise-security/devise-security) project.

When focusing on collaborating, CLI was still incredibly useful. The command I used most was `gh pr checkout` for checking out the code related to a Pull Request. This is also my most-used CLI command for my projects at work as well.

As you may be able able to guess from the name, the `pr checkout` command will, similar to `git-checkout` (or `git-switch`), will checkout the code for the Pull Request it is given as an argument. The Pull Request can be given as a number, the URL, or the branch name.

```bash
Check out a pull request in git

USAGE
  gh pr checkout {<number> | <url> | <branch>} [flags]
```

This is my go-to way for checking out code in a Pull Request that I am code reviewing.

The main reason I find myself using `pr checkout` over regular `git-checkout` is the fact that it will `fetch` the latest version of the branch from your remote and `pull` it when switching to the branch. At a minimum, this saves some typing, but, for me, this more often saves me from the trouble of forgetting to pull in the latest changes and not having the most up-to-date code.

When working on an open source project, I found CLI's `pr checkout` command became even more powerful as it will checkout branches across forks of the project.

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

To be perfectly honest, I am not 100% sure how I would replicate this workflow manually. My guess would be it would involve adding the contributor's fork to my list of remotes and then checking out their branch. Fortunately, with CLI, I don't have to worry about this.


Here's how to do it: https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/checking-out-pull-requests-locally
It seems like it's not pulling in the other persons fork and branch per se, but GitHub may be doing something special with `refs`

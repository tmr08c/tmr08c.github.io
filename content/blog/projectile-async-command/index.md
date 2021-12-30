---
title: "Running Async Shell Commands with Projectile"
date: 2021-03-28 13:15:02
categories: ["emacs", "quick tip"]
---

In this quick tip post, I wanted to highlight a function from the [projectile](https://github.com/bbatsov/projectile) project that I have been using a lot lately.

If you are unfamiliar, `projectile` is one of the many useful [`bbastov` projects](https://github.com/bbatsov). It's aim is to provide functionality around working with projects in Emacs (e.g., finding a file within a project).

The function I have want to highlight is [` projectile-run-async-shell-command-in-root`](https://github.com/bbatsov/projectile/blob/24de2940a8a1f46a7715175a66be67733f1c8fa8/projectile.el#L4046-L4050). As the name explains, it will asynchronously run a shell command from your projects current root (there is also a [non-async version](https://github.com/bbatsov/projectile/blob/24de2940a8a1f46a7715175a66be67733f1c8fa8/projectile.el#L4039-L4043)).

This function will prompt you for a shell command, open a split window, and print the output in the new window.

- Because it's async it does not tie up Emacs like a synchronous command may
- It does not require opening a term or shell split and `cd`ing to your projects root

I have found this most useful for small scripts we have in our projects at work. For example, we have a script to "setup" the project (pull in recent dependencies, perform database migrations) and one to run everything that is checked CI. The ability to kick off these scripts and track their progress while optionally doing something else has been great.

As a [Doom Emacs](https://github.com/hlissner/doom-emacs) user (using Vim bindings), this command can be run with `<SPC> p &`.

```
Key Bindings
doom-leader-map p &
doom-leader-project-map &
general-override-mode-map <emacs-state> M-SPC p &
general-override-mode-map <insert-state> M-SPC p &
general-override-mode-map <motion-state> SPC p &
general-override-mode-map <normal-state> SPC p &
general-override-mode-map <visual-state> SPC p &
projectile-command-map &
```

# TODO

- Maybe record gif?

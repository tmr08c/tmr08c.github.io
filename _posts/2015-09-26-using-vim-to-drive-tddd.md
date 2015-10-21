---
layout: post
title:  "Using Vim to Drive TDD"
date:   2015-09-26 09:43:02
categories: vim
---

A lot of recent developments in my programming career have been inspired by [thoughtbot](https://thoughtbot.com/), this is the result of following their blog and listening to their podcasts. Two of the major changes I have been working on are, adopting Vim and testing driving my development. Fortunately these activities are not mutually exclusive and I am able to develop my abilities in both simultaneously.

## Specs Should Run in Vim

In order to actually follow TDD I need to be running my tests constantly. If I want to run my tests constantly I need it to be easy -  preferably not requiring any sort of context switch. Running specs in Vim would help me to avoid the context switch but could it be easy?

Thanks to the thoughtbot team and their Vim plugin, [vim-rspec](https://github.com/thoughtbot/vim-rspec), it can be!

This plugin adds functions for running all specs, the entire current spec file, the nearest spec(s), and to re-run the most recent spec. These functions can be [mapped to leader commands](https://github.com/thoughtbot/vim-rspec#key-mappings) to allow you to run them quickly.

<img src='/images/run_spec_default.gif'></img>

To really get into the TDD flow I run specs basically anytime I am in normal mode for more than a few seconds, with this plugin was essentially spamming my new leader commands. I find that unless I am quickly editing a recent change I generally "sit" in normal mode while I am thinking of my next move. This thinking time lends it self to running specs.

While I am sure most developers trying to show off the benefits of TDD don't actually run the specs as often as they do for demos I have found there is some small pleasure in this habit. I will run specs even when I know what the failure is going to be (there is a certain pride in knowing what is going to cause the failure). However, when you do this too much you do run the risk of actually slowing down development.  I was no longer sitting in Normal mode thinking up solutions, instead I was staring at RSpec running, losing context of where I was in my code. I wanted to be able to run my specs **and** still browse my code.

This brings me to my next level of Vim TDD - running specs in a new window within the current buffer.

## Specs Shouldn't Stop Development

To be fair the realization that I didn't have to lose focus on Vim while running specs also came from thoughtbot. While watching a [talk by Ben Orenstein](https://www.youtube.com/watch?v=PU3qIVAO9aM) he pointed out that when he ran specs the output came up in a little pop-up Vim window **and** if they all passed the window would disappear.

I knew what I wanted to do was possible, now all I had to do was figure out how it was done.

Luckily Ben Orenstein is a generous guy and [shares his dotfiles](https://github.com/r00k/dotfiles) and even has a [nice comment](https://github.com/r00k/dotfiles/blob/master/vimrc#L280-L282) letting me know where the test running stuff is.

From the`vimrc` of Ben Orenstein:

```vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Test-running stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Now using thoughtbot/vim-rspec and tpope/dispatch.

let g:rspec_command = "Dispatch bin/rspec {spec}"
```

Since I was already using vim-rspec it looks like I needed to look into [Dispatch](https://github.com/tpope/vim-dispatch). Dispatch can be used to run a lot of different things either in pop-up quick fix windows or in the background. The README provides various examples such as building Java programs and running tests.

Luckily I already had the key to what I needed to do, `let g:rspec_command = "Dispatch bin/rspec {spec}"`, and after I install Dispatch with Vundle all that I had to do was add that line to my `vimrc` file.

Now when I run my specs a new window split is created.

<img src='/images/run_spec_dispatch.gif'></img>

*Note: The behavior of the quick fix window is dependent upon your environment. Check the [README](https://github.com/tpope/vim-dispatch#foreground-builds) for how each environment works.*

For those interested in where `let g:rspec_command = "Dispatch bin/rspec {spec}"` comes from - it is a setting for vim-rspec. It tells the plugin how you want to run your specs and [can be customized](https://github.com/thoughtbot/vim-rspec#custom-command) to run any command. I would stumble upon this when I decided to take my next leap into making specs run faster.

## Specs Should Run Fast

I tend to be too accepting of things being slow and write it off as providing a mental break. Fortunately the Vim contagion has spread to other developers in my office, some of whom have been less accepting of spec run times.

One of my coworkers was sharing his distress over slow running specs and I mentioned he should look into [Zeus](https://github.com/burke/zeus). I had investigated Zeus in the past in the hopes that it would help speed up the workflow of our Quality Assurance engineer. When running the full suite I didn't notice a significant improvement in time and decided it wasn't worth the setup for QA. At that time I was not practicing TDD and the majority of my test runs for full suite runs so decided to stop pursuing Zeus for my own use as well.

When my coworkers said he wanted faster running specs I remembered the hopes of Zeus and suggested he try it, adding in the caveat that I didn't have luck in the past. A short while later our Hipchat developer chat had a beautiful gif - a near instant test run.

![beautiful_spec_run](https://s3.amazonaws.com/uploads.hipchat.com/136875%2F992262%2FJdyA2zXyo3EnLjj%2Fzeus.gif)

This was a welcome surprise!

### Setting Up Zeus

What I had overlooked was that Zeus preloads the environment, saving the initial time required to load the whole Rails app; the time where normal spec runs seem to "hang". With a long running test suite this time savings feels negligible. With a single spec file, however, the time to boot up the Rails environment is actually longer than the run itself. With this initial boot time out of the picture running a spec with Zeus feels instantaneous.

On top of everything setting up Zeus is very easy, it's just a gem so `gem install zeus` is *almost* all you need.

In order to keep your Rails environment loaded you need to keep Zeus running. This is done by running `zeus start` in your project's root directory. This can be done in a new terminal tab or, better yet, a new [tmux](https://tmux.github.io/) window.

Now, with Zeus running, you can run your specs with `zeus test path/to/spec` or the shortcut `zeus t`.

### Running Zeus in Vim

If we hop back into Vim and run our specs we'll see they aren't taking advantage of Zeus. This is where our friend `g:rspec_command` comes in to play again. As I mentioned before, this is what tells vim-rspec how to run you specs. Earlier we told it to run using Dispatch, we now want it to use Dispatch **and** Zeus.

This is actually common enough that the thoughtbot team has instructions on how to do this in the [README](https://github.com/thoughtbot/vim-rspec#custom-command) for vim-rspec:

```
let g:rspec_command = "compiler rspec | set makeprg=zeus | Make rspec {spec}"
```

From [what I gather](https://github.com/tpope/vim-dispatch/issues/10) this tells Dispatch to run like it's using RSpec as its compiler, `compiler rspec`. It seems this helps to have properly formatted output. You then tell it to actually run RSpec commands using Zeus, `set makeprg=zeus`. I could also be completely making this up and not grasp the wizardry of the great [tpope](https://github.com/tpope).

Whatever the case with the above line in my vimrc and Zeus running in its own tmux window I now have test that run quickly, in their own small pane, and all with only a few keystrokes.

<img src='/images/run_spec_zeus.gif'></img>

## Conclusion

There are numerous excuses for not following some practice and TDD is no exception. Generally when exploring anything new I try not to invest too much time "tricking it out" until I see how I like the base concept. I think for TDD this can be detrimental to sticking to it. If your specs are difficult to run and running them is noticeably slow it is hard to see why anyone would advocate TDD. If you are considering following the TDD workflow do yourself a favor and spend a little extra time up front to have an environment that is conducive to a successfully TDD workflow.

---
layout: post
title:  "Using Vim to Drive TDD"
date:   2015-11-08 15:19:02
categories: vim
---

A lot of recent developments in my programming career have been inspired by [thoughtbot](https://thoughtbot.com/). This began as the result of using their [gems](https://github.com/thoughtbot/) and finding useful information on [their blog](https://robots.thoughtbot.com/) and has escalated to listening to their [numerous](http://bikeshed.fm/) [podcasts](http://giantrobots.fm/) and striving to maintain thoughtbot-level standards for my development team.

Two of the major changes I have been working on are adopting Vim and testing driving my development. Fortunately, these activities are not mutually exclusive, and I have been able to work on developing both skill sets at the same time.

## Specs Should Run in Vim

To truly embrace the TDD lifestyle you should be running your tests constantly. In order to follow this practice it should be easy to run tests, preferably not requiring any sort of context switch; bonus points if you can run them from where you are writing your code. For me, this meant I needed to be able to run my tests from within Vim.

Thanks to the thoughtbot team and their Vim plugin, [vim-rspec](https://github.com/thoughtbot/vim-rspec), this was very simple to achieve.

This plugin adds functions that allows you to:

* Run your entire test suite
* Run all specs in the current file
* Run the spec under your cursor (or nearest spec(s))
* Rerun the last spec-running command

These functions can all be [mapped to leader commands](https://github.com/thoughtbot/vim-rspec#key-mappings), allowing you to run them with just a few keystrokes.

<center>
  <img src='/images/run_spec_default.gif'></img>
</center>

As I further developed my TDD prowess, I began running my specs even when I knew that they were still going to fail; being able to reliably predict the next error meant I was on the right track.

I was now facing the issue of running my specs so often that I was seeing more of RSpec's output than my own code. This is because, by default, vim-rspec runs the specs in a new buffer.  I was no longer sitting in Normal mode thinking up solutions, instead I was staring at RSpec running, losing my train of thought.

What I needed to be able to do was run my specs **and** still browse my code. This brings me to my next level of Vim TDDing - running specs in a new window within the current buffer.

## Specs Shouldn't Stop Development

To be fair, the concept that I didn't have to lose focus on Vim while running specs was also inspired by thoughtbot. While watching a [talk](https://www.youtube.com/watch?v=PU3qIVAO9aM) by [Ben Orenstein](http://www.benorenstein.com/), he pointed out that when he ran specs, the output came up in a little pop-up Vim window and, if they all passed, the window would disappear.

I now knew what I wanted and just had to figure out how to do it.

Luckily, Ben Orenstein [shares his dotfiles](https://github.com/r00k/dotfiles) and even has a [helpful comment](https://github.com/r00k/dotfiles/blob/master/vimrc#L280-L282) letting us know where the test running stuff is:

```vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Test-running stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Now using thoughtbot/vim-rspec and tpope/dispatch.

let g:rspec_command = "Dispatch bin/rspec {spec}"
```

Since I was already using vim-rspec it looks like I needed to look into [Dispatch](https://github.com/tpope/vim-dispatch).

From Dispatch's README:

> Leverage the power of Vim's compiler plugins without being bound by synchronicity. Kick off builds and test suites using one of several asynchronous adapters (including tmux, screen, iTerm, Windows, and a headless mode), and when the job completes, errors will be loaded and parsed automatically.

Basically, Dispatch can be used to run a lot of different things, either in a pop-up quickfix window or in the background.

Thanks to Ben Orenstein I already had the key to what I needed to do:

`let g:rspec_command = "Dispatch bin/rspec {spec}"`

and, after installing Dispatch with Vundle, all I had to do was add that line to my `vimrc` file.

Now when I run my specs a new window split is created.

<center>
  <img src='/images/run_spec_dispatch.gif'></img>
</center>

*Note: The behavior of the quickfix window is dependent upon your environment. Check the [README](https://github.com/tpope/vim-dispatch#foreground-builds) for how each environment works.*

For those interested in where `let g:rspec_command = "Dispatch bin/rspec {spec}"` comes from - it is a setting for vim-rspec. It tells the plugin how you want to run your specs and [can be customized](https://github.com/thoughtbot/vim-rspec#custom-command) to run any command. We will explore this command and its flexibility further in the next section.

## Specs Should Run Fast

I tend to be too accepting of things being slow and write it off as providing time for my brain to rest. Fortunately, the Vim contagion has spread to other developers in my office, some of whom have been less accepting of spec run times.

One of my coworkers was sharing his distress over slow running specs and I mentioned he should look into [Zeus](https://github.com/burke/zeus). I had investigated Zeus in the past in the hopes that it would help speed up the workflow of our Quality Assurance engineer. When running the full suite I didn't notice a significant improvement in time and decided it wasn't worth the setup for QA (even though, as we will see, setup is simple). Having not been practicing TDD at the time, I wasn't too worried about test run times for myself and further exploration of Zeus ceased.

When my coworkers said he wanted faster running specs I remembered the hopes of Zeus and suggested he try it, adding in the caveat that I didn't have luck in the past. A short while later our Hipchat developer chat had a beautiful gif - a very promising before and after.

<center>
  ![beautiful_spec_run](https://s3.amazonaws.com/uploads.hipchat.com/136875%2F992262%2FJdyA2zXyo3EnLjj%2Fzeus.gif)
</center>

This was a welcome surprise!

What I had overlooked was that Zeus preloads the environment, saving the initial time required to load the whole Rails app; the time where normal spec runs seem to "hang". With a long running test suite this time saving feels negligible. With a single spec file, however, the time to boot up the Rails environment is actually longer than the run itself. With this initial boot time out of the picture, running a single spec file with Zeus is blazing fast.

### Setting Up Zeus

On top of everything, setting up Zeus is very easy. Since Zeus is just a gem, `gem install zeus` is *almost* all you need.

In order to keep your Rails environment loaded you need to keep Zeus running. This is done by running `zeus start` in your project's root directory. This can be done in a new terminal tab or, better yet, a new [tmux](https://tmux.github.io/) window.

Now, with Zeus running, you can run your specs with `zeus test path/to/spec`.

Check out the README to see [what else you can run with Zeus](https://github.com/burke/zeus#usage).

### Running Zeus in Vim

If we hop back into Vim and run our specs we'll see they are still slow to run. This is where our friend `g:rspec_command` comes into play again. As I mentioned before, this is what tells vim-rspec how to run your specs. Earlier we told it to run using Dispatch, we now want it to use Dispatch and Zeus.

This is actually common enough that the thoughtbot team has instructions on how to do this in the [README](https://github.com/thoughtbot/vim-rspec#custom-command) for vim-rspec:

```
let g:rspec_command = "compiler rspec | set makeprg=zeus | Make rspec {spec}"
```

From [what I gather](https://github.com/tpope/vim-dispatch/issues/10) this tells Dispatch to run like it's using RSpec as its compiler, `compiler rspec`. It seems this helps to have the output formatted properly. You then tell it to actually run RSpec commands using Zeus, `set makeprg=zeus`. I could also be completely making this up and simply not be able grasp the wizardry of the great [tpope](https://github.com/tpope).

Whatever the case is, with the above line in my vimrc and Zeus running in its own tmux window I now have test that run quickly, in their own small pane, and all with only a few keystrokes.

<center>
  <img src='/images/run_spec_zeus.gif'></img>
</center>

## Conclusion

There are numerous excuses for not following some best practice and TDD is no exception. Generally when exploring anything new I try not to invest too much time "tricking it out" until I see how I like the base concept. I think for TDD this can be detrimental to sticking to it. If your specs are difficult to run and running them is slow, it is hard to see why anyone would advocate TDD.

If you are considering following the TDD workflow do yourself a favor and spend a little extra time up front to have an environment that is conducive to successfully TDDing. It doesn't have to be Vim, but it should be fast and easy.

Are you practicing TDD? What does your workflow look like? Please share any tips or tricks in the comments section below.

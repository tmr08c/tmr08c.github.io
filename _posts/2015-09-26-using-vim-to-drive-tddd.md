---
layout: post
title:  "Using Vim to Drive TDD"
date:   2015-09-26 09:43:02
categories: vim
---

A lot of recent developments in my programming career have been inspired by [thoughtbot](https://thoughtbot.com/) namely through their various blog posts and podcasts. Two of the major changes I have been working on are adopting Vim and testing driving my code. I recently have worked on combining these two new passions together.

## Specs Should Run in Vim

The ability to run sepcs in Vim  was something that I quickly realized needed to be into my workflow. I think this came to me so quickly because, again, I am cyber-stalking the thoughtbot team and they are the authors of the [vim-rspec](https://github.com/thoughtbot/vim-rspec) plugin.

With this plugin was I getting into the flow of spamming my new of leader command to run specs. To really get into the TDD flow I have been working on building up the habit of running specs basically anytime I would be in normal mode for more than a few seconds. I find that unless I am quickly editing a recent change I generally "sit" in normal mode if I am thinking. TDD is great because while I am thinking I can get some feedback.

### TODO - Decided if I want to have any sort of HOWTO for install

I am sure most developers trying to show off the benefits of TDD don't actually run the specs as often as they do for conference talks or demos but there is some small pleasure in even just seeing the error change. It's almost as satisfying to know what the next failure is going to be as it is to actually have the spec pass. However, when you do this too much you do run the risk of actually slowing down developement. I found running specs wasn't allowing me to think of the next problem because I would already know what I neede to next. Instead I was basically just waiting for control of my Vim session again.

This brings me to my next level of Vim TDD - I didn't want to have the watch RSpec run in a new frame.

## Specs Shouldn't Stop Development

To be fair this realization that I didn't have to lose focus on Vim while running specs also came from thoughtbot. While watching [a talk by Ben Orenstein](https://www.youtube.com/watch?v=PU3qIVAO9aM) he pointed out that when he ran specs the output came up in a little pop-up Vim window **and** if they all passed the window would disappear. The first time I watched this video I thought, "Oh cool, maybe I should look into that" but didn't, not for a while at least.

When I began running tests like it was a nervous tick I then started to realize it would be nice if I could still move around while I was running the test. If I could do that I could begin working on fixing the expected error. If the expexted error did in fact come up I could finish my tpying and run the spec again. If I got a different error than TDD was paying off and I could work on fixing the new, unexpected error.

I knew what I wanted to do was possible, now all I had to do was figure out how it was done.

Luckily Ben Orenstein is a generous guy and [shares his dotfiles with the work on Github](https://github.com/r00k/dotfiles) and even has a [nice comment](https://github.com/r00k/dotfiles/blob/master/vimrc#L280-L282) letting me know where the test running stuff is.

```vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Test-running stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Now using thoughtbot/vim-rspec and tpope/dispatch.

let g:rspec_command = "Dispatch bin/rspec {spec}"
```

Since I was already using vim-rspec it looks like I needed to look into [Dispatch](https://github.com/tpope/vim-dispatch). Dispatch can be used to run a lot of different things either in pop-up quick fix windows or in the background. The README provides various examples such as building Java programs and running tests.

Luckily I already had the key to what I needed to do, `let g:rspec_command = "Dispatch bin/rspec {spec}"`, and after I install Dispatch with Vundle all that I had to do was add that line to my `vimrc` file.

Now when I run my specs a new pane pops up as they run.

For those interested in where `let g:rspec_command = "Dispatch bin/rspec {spec}"` comes from - it is a setting for vim-rspec. It tells the plugin how you want to run your specs and [can be customized](https://github.com/thoughtbot/vim-rspec#custom-command) to run any command. I would stumble upon this when I decided to take my next leap into making specs run faster.

## Specs Should Run Fast

When driving your development by tests it becomes draining when you find yourself spending more time waiting for your tests to run than you are writing code. I tend to be too accepting of things being slow and write it off as providing a mental break. Fortunately the Vim contagion has spread to other developers in my office, some of whom have been less accepting of spec run times.

One of my coworkers was sharing his distress over slow running specs and I mentioned he should look into [Zeus](https://github.com/burke/zeus). I had investigated Zeus in the past in the hopes that it would help speed up the workflow of our Quality Assurance engineer. When running the full suite I didn't notice a significant improvement in time and decided it wasn't worth the setup for QA. At that time I was not practicing TDD and the majority of my test runs for full suite runs as well so decided to stop pursuing Zeus for my own use as well.

When my coworkers said he wanted faster running specs I remembered the hopes of Zeus and suggested he try it, adding in the caveat that I didn't have luck in the past. A short while later our  Hipchat developer chat had a beautiful gif - a near instant test run.

![beautiful_spec_run](https://s3.amazonaws.com/uploads.hipchat.com/136875%2F992262%2FJdyA2zXyo3EnLjj%2Fzeus.gif)

This was a welcome surprise!

### Setting Up Zeus

What I had overlooked was that Zeus preloads the environment, saving the initial time required to load the whole Rails app. The time where normally the spec seems to sort of just hang. The longer the test suite the more negligible this time saving is. With a single spec file, however, the time to boot up the Rails environment is actually the majority of the time so when running a spec with Zeus they run nearly instantly.

On top of everything setting up Zeus is very easy, it's just a gem so `gem install zeus` is *almost* all you need.

Zeus preloads your environment for you, in order to do this and keep it loaded you need to keep Zeus running with `zeus start` in your project's root directory. This can be done in a new terminal tab or, better yet, a new [tmux](https://tmux.github.io/) window.

### Insert Zeus start up gif here

Now, with Zeus running, you can run your specs with `zeus test path/to/spec` or the shortcut `zeus t`.

### Running Zeus in Vim

If we hop back into Vim and run our specs we'll see they aren't taking advantage of Zeus and will feel slow in comparison. This is where our friend `g:rspec_command` comes in to play again. As I mentioned before this is what tells vim-rspec how to run you specs. Earlier we told it to run using Dispatch, we now want it to use Dispatch **and** Zeus.

This is actually common enough that the thoughtbot team has instructions on how to do this in the [README](https://github.com/thoughtbot/vim-rspec#custom-command) for vim-rspec:

```
let g:rspec_command = "compiler rspec | set makeprg=zeus | Make rspec {spec}"
```

From [what I gather](https://github.com/tpope/vim-dispatch/issues/10) this tells Dispatch to run like it's using RSpec as its compiler, `compiler rspec`. It seems this helps to have proper output. You then tell it to actually run RSpec commands using Zeus, `set makeprg=zeus`. I could also be completely making this up and not grasp the wizardry of the great [tpope](https://github.com/tpope).

Whatever the case with the above line in my vimrc and Zeus running in its own tmux window I now have test that run quickly, in their own small pane, and all with only a few keystrokes.

## Conclusion

There are numerous excuses for not following some practice and TDD is no exception. Generally when exploring anything new I try not to invest too much time "tricking it out" until I see how I like the base concept. I think for TDD this can be detrimental to sticking to it. If your specs are difficult to run and running them is noticeably slow it is hard to see why anyone would advocate TDD. If you are considering following the TDD workflow do yourself a favor and spend a little extra time up front to have an environment that is conducive to a successfully TDD workflow.

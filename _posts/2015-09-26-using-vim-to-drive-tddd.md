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

For those interested in what `let g:rspec_command = "Dispatch bin/rspec {spec}"` it is a setting for vim-rspec. It tells the plugin how you want to run your specs and [can be customized](https://github.com/thoughtbot/vim-rspec#custom-command) to run any command. I would stumble upon this when I decided to take my next leap into making specs run faster.

## Specs Should Run Fast

I tend to be too accepting of things being slow and write it off as providing a mental break. Fortunately the Vim contagion has spread to other developers in my office, some of whom have been less accepting of slow specs.

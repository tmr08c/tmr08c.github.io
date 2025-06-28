---
date: '2022-10-31T18:08:13'
slug: alt-escape-vim
tags:
- vim
- changed opinion
title: Changed Opinions - Alternative Escape Sequence in Vim
---

In this post, I provide a brief background on using alternatives to the <kbd>ESC</kbd> key in Vim (and Vim-like emulators) and how my opinion has recently changed on one of the options.

## Why did they choose the <kbd>ESC</kbd> key?

When using Vim (out of habit, I will be using Vim throughout this post, but I believe everything also applies to its predecessor, Vi), you will inevitably wonder, 'why did they choose the escape key?'

As a modal editor, you constantly switch modes during your editing sessions. For transitioning between instert and normal mode, the default key is <kbd>ESC</kbd>. On modern QWERTY-style keyboards, the <kbd>ESC</kbd> is in the top-left of the keyboard, a stretch that, for my hands, requires moving my left hand away from the home row to reach. As a keyboard-driven environment, the requirement to move so far away from the home row is not ideal.

According to [Vim wiki](https://vim.fandom.com/wiki/Avoid_the_escape_key), choosing the escape key resulted from the keyboard layout used during development, that of the AMD-3A terminal. With this layout, the <kbd>ESC</kbd> key is where <kbd>TAB</kbd> is on a QWERTY keyboard, a more easily accessible location.

<figure>
    <a href="https://commons.wikimedia.org/wiki/File:KB_Terminal_ADM3A.svg#/media/File:KB_Terminal_ADM3A.svg"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/KB_Terminal_ADM3A.svg/1200px-KB_Terminal_ADM3A.svg.png" alt="KB Terminal ADM3A.svg"></a>
    <figcaption>
        <i>
        By No machine-readable author provided. 
        <a href="//commons.wikimedia.org/wiki/User:StuartBrady" title="User:StuartBrady">StuartBrady</a> 
        assumed (based on copyright claims). - No machine-readable source provided. Own work assumed (based on copyright claims)., 
        <a href="http://creativecommons.org/licenses/by-sa/3.0/" title="Creative Commons Attribution-Share Alike 3.0">CC BY-SA 3.0
        </a>,
        <a href="https://commons.wikimedia.org/w/index.php?curid=1048046">Link</a>
        </i>
    </figcaption>
</figure>

## What I've used

History lesson aside, I was fortunate enough to be exposed to ways of [avoiding the escape key](https://vim.fandom.com/wiki/Avoid_the_escape_key) early on in my Vim usage as well as the concept of updating <kbd>Caps Lock</kbd> to <kbd>Control</kbd>.

While the <kbd>Caps Lock</kbd> remapping advice seems to be more common in the Emacs community (as a way to [avoid 'Emacs pinky'](https://www.emacswiki.org/emacs/MovingTheCtrlKey)) it has proven more widely beneficial. Since [Emacs won the editor wars](http://trevorjim.com/how-emacs-beat-vi-in-the-editor-wars/), <kbd>Control</kbd> is a common modifier across UNIX-like systems and shortcuts like <kbd>Control</kbd> <kbd>a</kbd> (move to start of line) work throughout the OS.

Moving <kbd>Control</kbd> is also relevant because, on American English keyboards, <kbd>Control</kbd> <kbd>[</kbd> will send the <kbd>ESC</kbd> sequence to the operating system, making it equivalent to <kbd>ESC</kbd>. Not only is this mentioned in the [Vim manual](https://vimdoc.sourceforge.net/htmldoc/insert.html#i_CTRL-[), it is recommend as an easier-to-hit alternative:

    <Esc> or CTRL-[ End insert or Replace mode, go back to Normal mode.  Finish
                    abbreviation.
                    Note: If your <Esc> key is hard to hit on your keyboard, train
                    yourself to use CTRL-[.

While initially seeing the combination <kbd>Control</kbd> <kbd>[</kbd> may seem unintuitive, trying it out with a remapped <kbd>Caps Lock</kbd> will reveal it requires only a small movement of both pinky fingers. This easy-to-reach combination has led to <kbd>Control</kbd> <kbd>[</kbd> being my primary method of escaping into normal mode.

## Changed opinions

We now come to my changed opinion.

The importance of quickly moving into normal mode is not lost on the Vim community. As a result, many [alternative mapping suggestions](https://vim.fandom.com/wiki/Avoid_the_escape_key#Mappings) exist. Until recently, I thought some of these options would get in the way of daily work. Specifically, I was concerned with the mappings that suggest using two keys, such as `jj`, `jk`, or `kj`, in rapid succession. My concern with using these mappings was that I would unexpectedly be thrown into normal mode when typing a work that included these characters.

My concern was proven unnecessary when I realized [DOOM has been shipping with `jk` mapped ever since I started using it](https://github.com/doomemacs/doomemacs/blame/61d7200e649d005ce80df0b74a6ee47b4db0a9d0/modules/editor/evil/config.el#L326). The fact that I did not realize this mapping was set proved that it was an unintrusive choice. The trick to these mappings is that if the time between letters exceeds the [timeout](https://vimdoc.sourceforge.net/htmldoc/options.html#'timeout') the actual letters will be inserted into your document.

After becoming aware that the mapping was already set up for me and hadn't gotten in the way, I began using it with positive results. When already on the home row,<kbd>j</kbd><kbd>k</kbd> requires **no additional movement**. Even if I am not on the home row, using <kbd>j</kbd><kbd>k</kbd> promotes me getting _back_ onto the home row if my fingers have strayed. While I do not have data, I sense that <kbd>j</kbd><kbd>k</kbd> has become my most-used mapping for transitioning into normal mode.

I now have three ways to enter normal mode: <kbd>ESC</kbd>, <kbd>Control</kbd><kbd>[</kbd>, and <kbd>j</kbd><kbd>k</kbd> and my fingers can choose based on whatever is most convenient for their current positions. Rather than being a landmine waiting to move me into normal mode at inopportune times, <kbd>j</kbd><kbd>k</kbd> has provided an additional time- and finger-saving mapping to enhance my editing experience.

_Full transparency: in writing this post, I did have <del>one</del> a few instances where typing `jk` put me into normal mode when trying to type it. I think this post is an exceptional case and therefore hasn't changed my opinion back._

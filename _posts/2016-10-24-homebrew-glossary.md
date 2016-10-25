---
layout: post
title:  "Glossary of Homebrew Terms"
date:   2016-10-24-21:05:02
categories: devops
---

While listening to the [most recent episode](https://changelog.com/podcast/223) of the [Changelog podcast](https://changelog.com/podcast), _Homebrew and Package Management with Mike McQuaid_, I began to realize how unfamiliar I was with the various terminology that [Homebrew](http://brew.sh) uses. I wasn't the only one feeling this way as, during the episode, one of the hosts, [Adam Stacoviak](https://twitter.com/adamstac), commented he thought there should be a glossary of Homebrew terms. This offhand comment inspired me to make a small glossary to keep for reference.

Before beginning, if you do not have Homebrew installed and you are using OSX go [here](http://brew.sh/) and set it up. If you use the terminal at all, this is a necessity. Homebrew is an OSX package manager. If you are familiar with Linux, Homebrew is similar to something like `apt-get`, `yum`, and `pacman`,.

## Formulae

While not defined in the podcast, formulae are the bread and butter of Homebrew. Formulae are the definitions of how to install the various command line packages/applications available through Homebrew. These are the things that make `brew install` work.

Take a look at the list of available formulae [in the Homebrew repo](https://github.com/Homebrew/homebrew-core/tree/master/Formula).

## Taps

A tap is a third party repository. Taps allows anyone to have a (git) repository with a collection of formulae that people can add to Homebrew and download packages from. Taps allow users to leverage Homebrew to download packages that are not maintained by the Homebrew team or are not a part of the default repository of formulae.

Mike gave examples of language specific taps that can contain dependencies that are common when working with a language. For example the [Python tap](https://github.com/Homebrew/Homebrew-python) "provides formulae to install Python libraries with external dependencies that pip doesn't know how to handle". The Homebrew organization has a few of these such as one for [Emacs](https://github.com/Homebrew/Homebrew-emacs) and one for [PHP](https://github.com/Homebrew/Homebrew-php).

Taps also appear to be a common way for organizations to bundle tools and libraries they build/use. Take a look at [thoughtbot's tap](https://github.com/thoughtbot/Homebrew-formulae) for an example.

See more in the [Homebrew tap documentation](https://github.com/Homebrew/brew/blob/master/docs/brew-tap.md).

## Casks

An extension of Homebrew, Homebrew Cask "extends Homebrew and brings its elegance, simplicity, and speed to macOS applications and large binaries alike." Essentially Casks are Homebrew packages for GUI applications. Casks allow you to leverage the command line to install applications such as Google Chrome and (at the time of writing) 3,381 other applications.

Wondering if you your favorite application is available via Cask? Take a look at the [list of Casks](https://github.com/caskroom/homebrew-cask/tree/master/Casks) or their nifty [search page](https://caskroom.github.io/search) to check out what is available.

See more at the [Homebrew Cask homepage](https://caskroom.github.io/).

## Brewfile

Similar to [Ruby's Gemfile](http://bundler.io/v1.5/gemfile.html) (a way to list Ruby gems necessary for a project), a `Brewfile` allows you to list Homebrew packages, Casks, taps, and even App Store applications to install on your system. `Brewfile`s are leveraged by scripts like thoughtbot's [laptop](https://github.com/thoughtbot/laptop) to quickly get new machines in a dev-ready state. `Brewfile`s can even be set up in project repositories to easily to get a new member's environment ready quickly.

Here is an example `Brewfile` from the [project's README](https://github.com/Homebrew/homebrew-bundle#usage):

```
cask_args appdir: '/Applications'
tap 'caskroom/cask'
tap 'telemachus/brew', 'https://telemachus@bitbucket.org/telemachus/brew.git'
brew 'imagemagick'
brew 'mysql', restart_service: true, conflicts_with: ['homebrew/versions/mysql56']
brew 'emacs', args: ['with-cocoa', 'with-gnutls']
cask 'google-chrome'
cask 'java' unless system '/usr/libexec/java_home --failfast'
cask 'firefox', args: { appdir: '~/my-apps/Applications' }
mas '1Password', id: 443987910
```

When in a directory with a `Brewfile` you can run `brew bundle` to install any missing dependencies.

You can create a `Brewfile` with your currently installed packages with the `brew bundle dump` command. After learning about this ability I ran the command on my system and added the resulting `Brewfile` to [my dotfiles](https://github.com/tmr08c/dotfiles/commit/1e9e37d7fd60343d33979bf6517197ed2a4b9260) repo for safe keeping.

Casks were one aspect of this episode that made me realize I wasn't fully leveraging Homebrew but when `Brewfile`s were brought up I realized I really needed to up my Homebrew game. Apparently this is old news and thoughtbot wrote about this [back in 2014](https://robots.thoughtbot.com/brewfile-a-gemfile-but-for-homebrew) but for those out there that also didn't know about them, I think `Brewfile`s can be a game changer for many teams.

Mike mentioned his own bootstrap script, [Strap](https://github.com/MikeMcQuaid/strap/) that leverages `Brewfile`s to bootstrap new systems. According to [his blog post](http://mikemcquaid.com/2016/06/15/replacing-boxen/) this may now be favorable within Github to their previous bootstrapping tool, [boxen](https://github.com/boxen/our-boxen).

See more in the [homebrew-bundle repository](https://github.com/Homebrew/homebrew-bundle).

## Keg

Apparently even within the Homebrew team the definition of Keg is debatable but it sounds like Kegs are where the packages are installed (which are in a path that contains the word `Cellar`). The way Homebrew does this is pretty cool because packages are install in their own, per-version directories and symlinked to `/usr/local/bin`.


Did I miss any terms? If so, please let me know in the comments!

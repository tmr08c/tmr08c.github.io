---
title: "My Personal Evolution of Troubleshooting Dependencies"
date: "2020-03-22T04:21:33.616Z"
categories: ["personal growth", "troubleshooting", "open source"]
---

During my last Innovation Friday at work, I decided I wanted to set up the [annotate gem](https://github.com/ctran/annotate_models) on one of our Rails applications (not particulary _innovative_, but a nice quality of life improvement). I expected this to be a quick win to start my day with, but, unsurprisngly, I underestimated things. While exploring the gem, I ran into a [few]() [issues](https://github.com/ctran/annotate_models/issues/778).

For this post, the issues themselves aren't important, but rather how I approached understanding and troubleshooting these issues. When reflecting on my day, I realized I took a different approach to troubleshooting issues with this gem than I generally have in the past. As I thought about it more, I thought about how my process for interacting with third-party libraries has changed over time and the evolution it took. I thought it could be interesting to write out the different stages I found myself in. Maybe this will help someone that is at a different stage in their journey. One great part about these strategies is that I continue to use all of them - they aren't mustually exclusive, they build on each other for working with more and more difficult troubleshooting sessions.

I believe I have been fortunate to get to work with open source technologies throughout my career. Without access to the open communities, my evolution would look very different.

## Stage one - Google

In the early days, I would often copy/paste the error I was seeing and hope I would end up finding a [StackOverflow](https://stackoverflow.com/) post with a helpful answer. This is a solid strategy, and something I still do. The difference from my early days and now was that if there wasn't a useful Google result the best I could do was tweak my Google search.

### Side note on language popularity

One thing to note about the above strategy is that it works well for [popular technology](https://www.tiobe.com/tiobe-index/). This post isn't about how to choose your tech stack and popularity definitely isn't the be-all end-all factor, but I thought it's worth pointing out why teammates may care about this.

## Stage two - GitHub

As I mentioned above, I've been fortunate to be able to work with open source technologies. For the most part this has meant that GitHub often ended up in my Google search results. Learning to interact with GitHub was hugely benefical and resulted in my continued evolution in interacting with and troubleshooting the tools I worked with.

### README and Wiki

As I became more comfortable with GitHub I moved beyond "help me Google 😫" and towards reading "the docs" a la [RTFM](http://www.readthefuckingmanual.com/). I put "docs" in quotes, becuase to start reading the docs meant reading (skimming?) a project's README. To be fair, most projects don't seem to have (or need) full-blown documentation websites; this is generally more useful for large projects (frameworks, languages, etc.).

I was familiar with READMEs as a tool for vetting and setting up new dependencies, but there's more to it than that! In my experience, many libraries will cover both common and more advanced use cases in the README. Some will even include warning/gotcha sections for things that may be unexpected. Since so many issues are small, end-user (see: me) problems, seeing expected usage and examples will often resolve the bulk of my troubleshooting sessions.

Sometimes, I will venture beyond the README and see if a project has a [GitHub wiki](https://help.github.com/en/github/building-a-strong-community/about-wikis). In practice, it seems most projects don't. Even for projects that have wiki pages, they can sometimes be difficult to navigate - while there is a sidebar that will list all pagese there is not default home page or sectional styling for maintainers to leverage. They are still worth checking out and I'm glad I learned about them and got in the habit of checking for them, but, in my experience, they tend to

I would like to develop the habit of reading a project's entire README when adding it to a project or working with it for the first time. It's usually really easy for me to read an overview or see the basic example and move on. I think this gives my brain a dopemeine hit of quickly learning something and moving on makes me feel like I am being efficient. I think this is one of those mental traps that can be easy to fall into - by sending a few more minute to read the whole document, I can save a lot more time down the road by knowing how the tool works more fully. One great way to speed up troubleshooting is to not get yourself into trouble in the first place.

### Issues

As I became more comfortable looking to GitHub to troubleshoot my issues, I learned to search through the project's [Issues](https://help.github.com/en/github/managing-your-work-on-github/about-issues) and sometimes [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).

As I write this, I realize my workflow with around this is actually a bit inefficient. Often I go to the Issues tab, remember to turn of the `is:open` filter. I then usually want to check for Pull Requests, so I tweak the search again. I think I would be better off going straight to the general search as it has tabs for Issues, Commits (which should be a superset of pull requests), wikis (above), **and** code (which is a spoiler for the next section):

<img src="./github-search.png" alt="github search page example" />

#### Creating Issues

If you have a problem on need help, before posting an Issue see if the project maintainers have anything in the README about their preference for support - some projects prefer posting on StackOverflow or leveraging a chat service for answering questions.

### Code

As aluded to above, the next stage of debugging for me is code hunting on GitHub. It took me a while to get comfortable doing this, and I wish I started this sooner. I think my hesitation to hunt through code was a form of [imposter syndrome](https://en.wikipedia.org/wiki/Impostor_syndrome) - I wasn't "good enough" to understand the code written by the "real" developers that make packages. Sure, there are projects with codebases that can be tricky to get my head around, but more often than not I can pretty quickly answer my troubleshooting question by hunting down the functionality I am trying to work with. GitHub is making this even easier as they continue to [roll out code navigation](https://github.blog/changelog/2019-11-13-code-navigation-is-now-available-for-all-go-python-and-ruby-repositories/) functionality.

I "plateaued" at this stage of debugging for a long time. I think there are two main reasons for this:

1. It's prety rare to need to get this far - a lot of problem-solving sessions come to an end from one the earlier stages.
1. If I find I am hitting a wall at this point, the next step for me ends up being evasive action (working on an alternative implementation or project to use).

### Bonus - Shortcuts

This section of troubleshooting has all be working within the GitHub interface. If you like keyboard shortcuts, checkout [GitHub's keyboard shortcuts](https://help.github.com/en/github/getting-started-with-github/keyboard-shortcuts). They provided shortcuts for quickly getting between pages, searching, and (my favorite) a fuzzy file finder.

## Stage Three - My Computer

As I mentioned in the beginning of the post, I believe I have "evolved" my third-party library problem-solving capabilities. This evolution is less about a change in skill and more a change in mentality. Inspired by the idea of developing [identify based habits](https://jamesclear.com/identity-based-habits) ("I'm the type of person that...") that align with the type of developer I would like to be (a developer that supports the open source community), I am striving to develop habits that I imagine prolific open source contributors would have. In this case, I imagine prolific open source contributors would comfortably clone repositories they are troubleshooting and debug the code as if it was there own code base.

So, driven by the long-term goal of giving back to the open source community, after going through my normal troubleshooting steps I was inspired to pull down the code and doing some debugging in my local development environment. Once I had the code on my computer, I gained asccess to all of my normal tools. With this new workflow, I can search code bases with tools like [`ripgrep`](https://github.com/BurntSushi/ripgrep) and navigate projects using all of the day-to-day niceities I have through my text editor configuration.

After getting over the mental hurdle to actually pull in the code, I found working from my normal, local development environemt.

<!-- easy it is to use GitHub to search through a codebase. It is also convenient to not have to pull down any code that I may not need long-term. On the other hand, -->

<!-- Above I mentioned that when I would hit a wall after using GitHub to search through code I would generally start looking for alternatives. Realistically, that's can't always happen (plus, I should work on my [grit](https://angeladuckworth.com/grit-book/), right?). -->

When working on it, having my local tools that I'm used to working with my own code

- Want to do this more
- Not sure how I'd do this for things like `rails/rails`

## Where to next?

- Leverage tools like `pry` to interact with the libraries and access documentation before context switching to GitHub
- Reading more thorough docs and guides (take this from team)
  - Mentioned above doing this for README, go beyond

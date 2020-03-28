---
title: "My Evolution of Troubleshooting Dependencies"
date: "2020-03-22T04:21:33.616Z"
categories: ["personal growth", "troubleshooting", "open source"]
---

During my last Innovation Friday at work, I decided I wanted to set up the [annotate gem](https://github.com/ctran/annotate_models) on one of our Rails applications (not particularly _innovative_, but a nice quality of life improvement). I expected this to be a quick win to start my day with, but, unsurprisingly, I underestimated things. While exploring the gem, I ran into a [few](https://github.com/ctran/annotate_models/issues/778) [issues](https://github.com/ctran/annotate_models/issues/780).

For this post, the issues themselves aren't important, but rather how I approached understanding and troubleshooting them. When reflecting on my day, I realized I took a different approach to this process than I have in the past. As I thought about it more, I realized this change in approach has happened before and that, like most processes, it is something that as evolved with time. I thought it could be interesting to write out the different stages I found myself in over time.

-- Maybe this will help someone that is at a different stage in their journey learn about a new approach they could take, or provide others an opportunity to share with me their practices as an opportunity for me to continue to grow in this process.

-- One great part about these strategies is that I continue to use all of them - they aren't mutually exclusive, they build on each other for working with more and more difficult troubleshooting sessions.

-- As I wrote this, I found it provided a way to reflect and evaluate my current processes. By writing what I do down, it became easier to find opportunities to improve my process. This process seems similar to what I've heard speakers refer to as [conference driven development](https://devdriven.by/conference/).

## Stage one - Google

In the early days, I would often copy/paste the error I was seeing and hope I would end up finding a [StackOverflow](https://stackoverflow.com/) post with a helpful answer. This is a solid strategy, and something I still do. The difference from my early days and now was that if there wasn't a useful Google result the best I could do was tweak my Google search.

### Side note on language popularity

One thing to note about the above strategy is that it works well for [popular technology](https://www.tiobe.com/tiobe-index/). This post isn't about how to choose your tech stack and popularity isn't the be-all-end-all factor, but I thought it's worth noting.

## Stage two - GitHub

I believe I have been fortunate to get to work with open source technologies throughout my career. Without access to open source communities, my evolution would look very different.

Since GitHub is "where open source communities live," working with open source tools means my early-stage Googling include GitHub in the results.

<img src="./github-tagline.png" alt="github tagline - where open source communities live" />

Learning to productively interact with GitHub was hugely beneficial and resulted in my continued evolution of interacting with and troubleshooting the tools I worked with.

### README and Wiki

As I became more comfortable with GitHub I moved beyond "😫 help me Google" and towards reading "the docs" a la [RTFM](http://www.readthefuckingmanual.com/). I put "docs" in quotes because, to start, reading the docs for me meant ~~reading~~ skimming a project's README. To be fair, most projects don't seem to have (or need) full-blown documentation websites; this is generally more useful for large projects (frameworks, languages, etc.).

I was familiar with READMEs as a tool for vetting and setting up new dependencies, but there's more to it than that! In my experience, many libraries will cover both common and more advanced use cases in the README. Some will even include sections withing warnings or common gotchas. Since so many issues are small, end-user (see: me) problems, seeing expected usage and examples was often all I needed to resolve an issue I was running into.

<!-- this section is a bit disjointed -->

Sometimes, I will venture beyond the README and see if a project has a set up a [wiki on GitHub](https://help.github.com/en/github/building-a-strong-community/about-wikis). In practice, it seems most projects don't. Even for projects that have wiki pages, they can sometimes be difficult to navigate - while there is a sidebar that will list all the pages, if there are a lot it only shows a preview list. Some projects will create a home page broken up into sections, but that isn't something that is provided out of the box, so many don't. With that said, wikis are still worth checking out since some projects do use them as a source of documentation.

#### Growth Opportunity

I would like to develop the habit of reading a project's entire README when adding it to a project or working with it for the first time. It's usually really easy for me to read an overview or see the basic example and move on. I think this gives my brain a dopamine hit of quickly learning something and moving on makes me feel like I am being efficient. I think this is one of those mental traps that can be easy to fall into. However, by spending a few more minutes to read the whole document I can save a lot more time down the road by knowing how the tool works more fully. One great way to speed up troubleshooting is to not get yourself into trouble in the first place.

### Issues

As I became more comfortable looking to GitHub to troubleshoot my issues, I learned to search through the project's [Issues](https://help.github.com/en/github/managing-your-work-on-github/about-issues) and sometimes [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).

#### Growth Opportunity

As I write this, I realize my workflow around this is a bit inefficient. Often I go to the Issues tab to search for some keywords (sometimes remembering to turn off the default `is:open` filter, sometimes not). If I don't find anything, I will want to check for Pull Requests too, so I tweak the search again (dropping `is:issue` from the search or going to the Pull Request tab).

I think I would be better off going straight to the general search bar in GitHub as it has tabs for Issues, Commits (which should be a superset of pull requests), wikis (above), **and** code (which is a spoiler for the next section):

<img src="./github-search.png" alt="github search page example" />

### Code

As alluded to above, the next stage of debugging for me is code hunting on GitHub. It took me a while to get comfortable doing this, and I wish I started this sooner. I think my hesitation to hunt through code was a form of [imposter syndrome](https://en.wikipedia.org/wiki/Impostor_syndrome) - I wasn't "good enough" to understand the code written by the "real" developers that make packages.

As I've worked to encorporate looking at code on GitHub into my toolbelt, I've found I usually can understand what's going on in the code. Sure, there are projects with codebases that can be tricky to get my head around, but more often than not I can pretty quickly answer my troubleshooting question by hunting down the functionality I am trying to work with.

One great part of the read code on GitHub workflow is that GitHub strives to make this easier functionality like [shortcuts](https://help.github.com/en/github/getting-started-with-github/keyboard-shortcuts) (one of my favoties being to invoke their fuzzy file finder) and are making this even easier as they [roll out code navigation](https://github.blog/changelog/2019-11-13-code-navigation-is-now-available-for-all-go-python-and-ruby-repositories/) functionality.

I "plateaued" at this stage of debugging for a long time. I think there are two main reasons for this:

1. It's prety rare to need to get this far - a lot of problem-solving sessions come to an end from one the earlier stages.
1. If I find I am hitting a wall at this point, the next step for me ends up being evasive action (working on an alternative implementation or project to use).

## Stage Three - My Computer

As I mentioned in the beginning of the post, I believe I have "evolved" my third-party library problem-solving capabilities. This evolution is less about a change in skill and more a change in mentality. Inspired by the idea of developing [identify based habits](https://jamesclear.com/identity-based-habits) ("I'm the type of person that...") that align with the type of developer I would like to be (a developer that supports the open source community), I am striving to develop habits that I imagine prolific open source contributors would have. In this case, I imagine prolific open source contributors would comfortably clone repositories they are troubleshooting and debug the code as if it was there own code base.

So, driven by the long-term goal of giving back to the open source community, after going through my normal troubleshooting steps I was inspired to pull down the code and doing some debugging in my local development environment. Once I had the code on my computer, I gained asccess to all of my normal tools. With this new workflow, I can search code bases with tools like [`ripgrep`](https://github.com/BurntSushi/ripgrep) and navigate projects using all of the day-to-day niceities I have through my text editor configuration.

After getting over the mental hurdle to actually pull in the code, I found working from my normal, local development environemt.

<!-- easy it is to use GitHub to search through a codebase. It is also convenient to not have to pull down any code that I may not need long-term. On the other hand, -->

<!-- Above I mentioned that when I would hit a wall after using GitHub to search through code I would generally start looking for alternatives. Realistically, that's can't always happen (plus, I should work on my [grit](https://angeladuckworth.com/grit-book/), right?). -->

When working on it, having my local tools that I'm used to working with my own code

- Want to do this more
- Not sure how I'd do this for things like `rails/rails`

### Creating Issues

If you've exhausted all other options, you may want to consider reaching out to the community. I would recommend checking a project's README to see if the project maintainers have a section on their preference for support - some projects prefer posting on StackOverflow or leveraging a chat service for answering questions over using something like GitHub Issues.

## Where to next?

- Leverage tools like `pry` to interact with the libraries and access documentation before context switching to GitHub
- Reading more thorough docs and guides (take this from team)
  - Mentioned above doing this for README, go beyond

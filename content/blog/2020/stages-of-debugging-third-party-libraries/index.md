---
title: "My Evolution of Troubleshooting Dependencies"
date: "2020-03-22T04:21:33.616Z"
categories: ["personal growth", "troubleshooting", "open source"]
---

Recently at work, I decided I wanted to set up the [annotate gem](https://github.com/ctran/annotate_models) for one of our Rails applications. I expected this to be a quick win to start my day, but, unsurprisingly, I [underestimated things](https://xkcd.com/1658/). While experimenting with some of the gem's [options](https://github.com/ctran/annotate_models/#options), I ran into a [few](https://github.com/ctran/annotate_models/issues/778) [issues](https://github.com/ctran/annotate_models/issues/780).

For this post, the issues themselves aren't important, but rather how I approached understanding and troubleshooting them. When reflecting on my day, I realized I did something different than I normally do when troubleshooting issues - I added a new step to my dependency debugging process.

In this post, I will cover the steps I have traditionally taken when troubleshooting an issue with a third-party library, the new process I have added to this workflow, and what else I think I can do to improve my process.

## Stage one - Google

Starting out as a developer, I would often copy/paste the error I was seeing and hope I would end up finding a [StackOverflow](https://stackoverflow.com/) post with a helpful answer. That was about the extent of my ability to troubleshoot problems with dependencies. 

I would be lying if I said I didn't still do some [StackOverlow driven development](https://meta.stackoverflow.com/questions/361904/what-is-stack-overflow-driven-development). One major difference between now and then is that I if there isn't a StackOverflow answer, I have other methods of solving my issue.

### Side note on language popularity

One thing to note about the above strategy is that it works well for [popular technology](https://www.tiobe.com/tiobe-index/). This post isn't about how to choose your tech stack and popularity isn't the be-all-end-all factor, but I thought it's worth pointing out.

## Stage two - GitHub

I believe I have been fortunate to get to work with open source technologies throughout my career. Without access to open source communities, I imagine my process would look _very_ different.

Since GitHub is "where open source communities live," working with open source tools means my early-stage Googling often includes GitHub pages in the search results.

<img src="./github-tagline.png" alt="github tagline - where open source communities live" />

Learning to productively interact with GitHub has been hugely beneficial and resulted in my continued evolution of interacting with and troubleshooting the tools I worked with.

### README and Wiki

As I became more comfortable with GitHub I moved beyond "😫 help me Google" and towards reading "the docs" a la [RTFM](http://www.readthefuckingmanual.com/). I put "docs" in quotes because, to start, reading the docs for me meant ~~reading~~ skimming a project's README. 

I was familiar with READMEs as a tool for vetting and setting up new dependencies, but learned there's more to them than that. In my experience, many libraries will cover both common and more advanced use cases in the README. Some will even include sections with warnings or common gotchas. Since so many issues are small, end-user (see: me) problems, seeing expected usage and examples is often all that is needed to resolve them.

If a README doesn't have enough information to solve your problem, see if the project has set up a [wiki on GitHub](https://help.github.com/en/github/building-a-strong-community/about-wikis). It seems that in practice most projects haven't, but it is always worth checking during a debugging session as wiki pages will often be more detailed than the README.

#### Growth Opportunity

I would like to develop the habit of reading a project's entire README when adding it to a project or working with it for the first time. It's all too common for me to simply read an overview and stop there. I believe I am falling into some sort of mental trap - my brain receives a dopamine hit from quickly learning something and by moving on I tell myself I am being efficient. However, by spending a few more minutes reading the whole document I expect I can save a lot more time long-term by having a better understanding of how the tool works. One great way to speed up troubleshooting is to not get yourself into trouble in the first place 🤯.

### Issues

As I became more comfortable looking to GitHub to troubleshoot my issues, I learned to search through the project's [Issues](https://help.github.com/en/github/managing-your-work-on-github/about-issues) and [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).

Issues are similar to StackOverflow questions in that they reveal problems others have had. If you're lucky, these Issues may include a resolution. Resolutions may come in a few forms:

1. Comments on the thread describing how to resolve the Issue.
1. Pull Requests within the project that involve code changes to resolve the Issue.
1. Issues or Pull Requests on _other_ projects that had the same issue. This may take some detective work 🕵️‍♂️, but if someone makes an Issue within their own repository linking to an Issue or Pull Request within the library's repository, you may be able to see see what they did in their project to solve the problem.

#### Growth Opportunity

As I write this, I realize my workflow around this is a bit inefficient. Often I go to the Issues tab to search for some keywords (sometimes remembering to turn off the default `is:open` filter, sometimes not). If I don't find anything, I will want to check for Pull Requests too, so I tweak the search again (dropping `is:issue` from the search or going to the Pull Request tab).

I think I would be better off going straight to the general search bar in GitHub as it has tabs for Issues, Commits (which should be a superset of pull requests), Wiki pages (above), **and** code (which is a spoiler for the next section):

<img src="./github-search.png" alt="GitHub search page example" />

### Code

As alluded to above, the next stage of debugging for me is code hunting on GitHub. It took me a while to get comfortable doing this, but I wish I started sooner. 

I think my hesitation to hunt through code was a form of [imposter syndrome](https://en.wikipedia.org/wiki/Impostor_syndrome) - I wasn't "good enough" to understand the code written by the "real" developers that make these libraries I am using. As I've worked to incorporate looking at code on GitHub into my toolbelt, I've found I usually can understand what's going on in the code. Sure, there are projects with codebases that can be tricky to get my head around, but, more often than not, I can read enough code to continue troubleshooting.

One great part of the "read code on GitHub" workflow is that GitHub strives to make this easy with functionality like [keyboard shortcuts](https://help.github.com/en/github/getting-started-with-github/keyboard-shortcuts) (one of my favorites being to invoke their fuzzy file finder) and are making this even easier as they [roll out code navigation](https://github.blog/changelog/2019-11-13-code-navigation-is-now-available-for-all-go-python-and-ruby-repositories/) functionality.

For a while, this was about as far as I would go with troubleshooting issues. I think I plateaued at this stage for two main reasons:

1. It's pretty rare to need to get this far - a lot of problem-solving sessions come to an end from one of the earlier stages.
1. I've found that if I am still hitting a wall at this point, I can start planing some evasive action (working on an alternative implementation or project to use) or pull in another set of eyes for help.

## Stage Three - My Computer

As I mentioned at the beginning of the post, I added another step to my troubleshooting during my most recent debugging session. This new step is less about a change in skill and more a change in mentality.

Inspired by the idea of developing [identify based habits](https://jamesclear.com/identity-based-habits) ("I'm the type of person that...") that align with the type of developer I would like to be, I am striving to develop habits that I imagine prolific open-source contributors would have. In this case, I imagine prolific open-source contributors would comfortably clone repositories they are troubleshooting and debug the code as if it was there own codebase.

And so, driven by the long-term goal of giving back to the open-source community, after exhausting my normal steps, I was inspired to pull down the code and do some debugging in my local development environment.

Once I got over the mental hurdle of actually pulling down the code, I found myself in a very comfortable place - the development environment in which I spend most of my working hours. I was able to leverage my favorite editor and the various auxiliary tooling I am comfortable with to view, search, and navigate the codebase. However, unlike on GitHub, reading and searching code is just a starting point. When I have the code locally, I can run it as well.

The ability to run and interact with the third-party code allowed me to update my project to point to the local version of the library I pulled in and do some [`puts` debugging](https://tenderlovemaking.com/2016/02/05/i-am-a-puts-debuggerer.html). This debugging session helped me properly identify why the gem wasn't working as expected, open [relevant](https://github.com/ctran/annotate_models/issues/778) [issues](https://github.com/ctran/annotate_models/issues/780), [pull](https://github.com/ctran/annotate_models/pull/779) [requests](https://github.com/ctran/annotate_models/pull/784), and understand how I could work around this issue in our project in the meantime.

This change was huge for me - not only did I identify an issue with a library I wanted to use and unstick myself at work, but by thinking "what would an open-source contributor do here," I was able to **be** an open-source contributor and submit multiple pull requests for the project I was working with.

### Creating Issues

If you've exhausted all other options, you may want to consider reaching out to the community. Before doing this, I would recommend checking a project's README to see if the project maintainers have a section on their preference for support - some projects prefer posting on StackOverflow or leveraging a chat service for answering questions over using something like GitHub Issues.

Asking for help can be hard. Personally, this is something I wish I was more comfortable with. It also seems [I'm not the only one](https://www.bikeshed.fm/236) (thanks to [@christoomey](https://ctoomey.com/) for being open about this). I've found it's easier to ask for help if I can show that I've done my due diligence trying to solve the problem for myself. By going through the above steps, it should be possible to include some additional context about the issue and show you've made the effort.

## Where to next

While I presume I will continue to start my code searching on GitHub since not having to context switch out of my browser (assuming I'm still troubleshooting in the same order) can save me some time when paired with the GitHub's search functionality, this experience has changed the way I think of things. I am looking forward to seeing how this new workflow improves not only my ability to resolve issues but also [write code](http://bloggytoons.com/code-club).

Going through the process of writing up my current workflow for this post has helped identify other practices that could be useful. I've attempted to highlight most of these ideas throughout the post but wanted to mention one other Ruby-specific practice I want to pick up: working with [`pry`](http://pry.github.io/). `pry` provides [source](https://github.com/pry/pry/wiki/Source-browsing) and [documentation](https://github.com/pry/pry/wiki/Documentation-browsing) browsing capabilities. This could help avoid the need to make the context switch to the browser to find documentation or do my initial GitHub code spelunking and keep me where I belong, in the terminal. 



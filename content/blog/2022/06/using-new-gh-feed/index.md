---
title: "Using the new GitHub feed"
date: "2022-06-30T18:08:13.265Z"
categories: ["github", "tools"]
---

In this post, I want to share how some of my usage patterns in GitHub has changed along with how GitHub itself has changed to reinforce these patterns. These changes are not related to coding or the project management aspect of GitHub, but instead are related to the "social" aspects. By "social" aspect of GitHub, I mean following other users and starring projects.

## Historial Usage

For most of my time on GitHub, I would primarily follow people I knew through work or Meetups, thinking of it through the lens of early social media usage.

My usage pattern for starring projects has changed over time. At some point, I started starring more projects. The problem was I didn't really _do_ anything with the list of starred projects. I did not find myself reviewing the list and leveraging projects and if I'm guessing if I ever thought, "I think I saw a project like this before," I struggled to actually find it again.

## The Change

My usage changes started with more gratuitously following users and starring repositories. This initial change in behavior started as I began to seek out Emacs configurations.

Due to the malleability and variation in usage patterns of Emacs, no two configurations are the same. I expect my usage pattern of Emacs to change over time and I want to have inspiration and references to look at, so I decided to star repos with Emacs config as I came across them. With the release of [lists](https://github.blog/changelog/2021-12-09-lists-are-now-available-as-a-public-beta/), I could "tag" the repositories I was starring as being Emacs related. This alleviates one of the problems of managing stars - by default, they are all mixed together. Now, I can have an `emacs` list that contains config and interesting packages (I may even find I eventually want to have a list of each of these).

Once I started starring more projects, I also decided to start following more people. I think this is mostly likely because [once you pop, the fun don't stop](https://www.youtube.com/watch?v=dmpQTpY76yI).

## GitHub Reinforcement

As more frequent starring turned into more frequent following, I somehow found myself periodically landing on my GitHub feed and finding interesting information.

When I followed fewer people and starred fewer repositories, my feed didn't provide much new information - it would mostly show information about my coworkers pushing code. I found this information to be too granular; I am better off simply going to the Pull Request tab for my project(s) to see what work is ready for review.

Now that I was following people and projects that I didn't work on, the feed would include information I was less likely to see - maybe I would learn about a new Emacs package or see that a new RC was released for Elixir. This was not game-changing information, but I found I was getting some value for a low cost.

I started looking at the feed page enough to actually click on the `For you (beta)` tab and use [GitHub's new feed](https://github.blog/2022-03-22-improving-your-github-feed/). Building upon the existing feed, the new feed seems to provide more second-order information. For example, seemingly based on what I currently follow, it will suggest new things to follow. And so goes the magic of training machine learning models (I am assuming that is what is powering the feed) - these suggestions can then feed into the next round; whether or not I engage with the suggestions, I am providing feedback that could be used to update my feed in the future.

Just like I am training their ML model (again, assuming), GitHub is also using reinforcement training on me. I am being rewarded for interacting with their platform more (more starts, follows), by being given a dopamine hit when seeing something interesting on my feed, pushing me to further engage...

## Social Media Feed

The self-feeding nature of the changes raise a slight alarm in my mind...is GitHub (or the evil M$ overlords) trying to keep my scrolling through an infinite feed as though they were a social media platform?

Maybe.

The social aspect of GitHub is not new. In the early days, [their tagline was "Social Coding"](https://1000logos.net/github-logo/). Social media was big then, people even used to admit to using Facebook. Jokes aside, the tagline made sense. GitHub made it easier to work on code with other people. Not only could you share access to code, but they also supported following other users and even used to have the [ability to DM people](https://stackoverflow.com/a/12687679)).

While [the open source community has had a positive impact on me](/2022/04/open-source-open-eyes/) I definitely [do not want GitHub to become a full blown social media platform](https://www.calnewport.com/blog/2018/08/23/a-brief-summary-of-the-social-media-reform-movement/).

I am often naive and sometimes optimistic, but I know companies improve their products because it can help them get and keep customers (which helps them make more money). I know GitHub gets value in having more people use their system. I can even see how, by following more developers and seeing more projects, you may get the feeling that everyone codes on GitHub, and not look at any other [code](https://about.gitlab.com/) [hosting](https://gitea.io/en-us/) [providers](https://bitbucket.org/product). With this all in mind, I am hopeful these changes could still be a [win-win-win](https://thefab20s.com/wp-content/uploads/2021/03/win-win-win-michael-scott-the-office.gif) situation. GitHub wins by increasing their network effect, GitHub users win by learning about interesting projects, and project creators win by getting exposure to their work (the feed will even highlight users and projects seeking sponsorship).

Something else to note about this feed is that I haven't found that it rapidly changes or provides value in checking it every day. In fact, over the course of writnig this post, GitHub-generated recommendations have been the same. I think this feed lends itself well to [Cal Newport's advice](https://www.calnewport.com/blog/2017/10/02/are-you-using-social-media-or-being-used-by-it/) of limiting social media interactions to a fixed schedule. I doubt you need to get as formal as scheduling time to check this feed, but I reference the idea to say, you would probably get all the value you need from the feed if you looked at it for a few minutes once a week.

## Conclusion

While working on this post, I realized that this could come off as though I am doing content marketing for GitHub. I am not. I was a bit surprised to realize I was actually starting to use the GitHub feed and thought maybe other people could find it useful too.

If you've been using GitHub for a while, you may have given up on some of these "social" features like I had. If that's the case, consider checking out the new feed and following more people and projects, maybe you will find some value too.

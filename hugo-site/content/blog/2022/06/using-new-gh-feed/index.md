---
date: '2022-06-30T18:08:13'
tags:
- github
- tools
title: Using the new GitHub feed
---

In this post, I want to share how some of my usage patterns in GitHub have changed. These changes are not related to the code or project management aspects of GitHub but the "social" aspects. By "social," I mean following users and starring projects.

## Historial Usage

To set the stage for how I've used the social aspects of GitHub for most of my career:

- I primarily followed people I had interacted with directly. As a result of this mindset, I had a small pool of people I followed, most of which were current or former coworkers.
- My usage pattern for starring projects has changed over time. At some point, I started starring more projects. The problem was I didn't _do_ anything with the list of starred projects; I did not find myself reviewing the list and would struggle to resurface relevant starred projects. I still periodcally starred things, but acknowledge it was basically going into `/dev/null`.

## The Change

My usage changes started with more gratuitously following users and starring repositories to have more Emacs configurations to reference.

Due to the malleability and variation in usage patterns of Emacs, no two configurations are the same. I expect my needs/wants of Emacs to change over time, so I decided to star repos with Emacs config as I came across them to reference later.

With the release of [lists](https://github.blog/changelog/2021-12-09-lists-are-now-available-as-a-public-beta/), I could "tag" the repositories I was starring as being Emacs related. This list concept alleviates one of the problems I had with starring in the past - knowing where to find things. Now, I can have an `emacs` list contraining config and interesting packages.

Shortly after starring more projects, I also started following more people; this is most likely because [once you pop, the fun don't stop](https://www.youtube.com/watch?v=dmpQTpY76yI) (a.k.a., I don't know why my brain does things sometimes).

## GitHub Reinforcement

As more frequent starring turned into more frequent following, I somehow found myself periodically landing on my GitHub feed and finding intriguing information.

When I followed fewer people and starred fewer repositories, my feed didn't provide much new information - it would mostly show information about my coworkers pushing code. I found this information too granular; I am better off simply going to the Pull Request tab for my project(s) to see what work is ready for review.

Now that I was following people I didn't know and projects I didn't work on, the feed would include information I was less likely to come across naturally. Maybe I would see the release notes for a library I used. This information is not life-changing, but I found I was getting some value for a low cost.

I started looking at the feed page enough to actually click on the `For you (beta)` tab and use [GitHub's new feed](https://github.blog/2022-03-22-improving-your-github-feed/). Building upon the existing feed, the new feed adds more second-order information. For example, seemingly based on what I currently follow, it will suggest new people/projects to follow. Through the magic of training machine learning models (assuming that is what is powering the feed), these suggestions can then feed into the next round. However I behave, the recommendation engine gets feedback to leverage for future viewings.

As I am training their ML model (again, assuming), GitHub is also using reinforcement training on me. For interacting with their platform more (more stars, follows), I am rewarde with a dopamine hit when seeing something interesting on my feed, pushing me to engage further...

## Social Media Feed

The self-reinforcing nature of the changes raises a slight alarm in my mind. Is GitHub (or the evil M$ overlords) trying to keep me scrolling through an infinite feed as though they were a social media platform?

Maybe?

The social aspect of GitHub is not new. In the early days, [their tagline was "Social Coding"](https://1000logos.net/github-logo/). Social media was big then; people even used to admit to using Facebook. Jokes aside, the tagline made sense. GitHub made it easier to work on code with other people. Not only could you share access to code, but they also supported following other users and even used to have the [ability to DM people](https://stackoverflow.com/a/12687679).

While [the open-source community has had a positive impact on me](/2022/04/open-source-open-eyes/) I definitely [do not want GitHub to become a full-blown social media platform](https://www.calnewport.com/blog/2018/08/23/a-brief-summary-of-the-social-media-reform-movement/).

I am often naive and sometimes optimistic, but I know companies improve their products because it can help them get and keep customers (which helps them make more money). I know GitHub gets value in having more people use their system. I can even see how, by following more developers and seeing more projects, they may invoke the feeling that everyone codes on GitHub, convincing me to not look at any other [code](https://about.gitlab.com/) [hosting](https://gitea.io/en-us/) [providers](https://bitbucket.org/product). With this all in mind, I am still hopeful these changes could be a [win-win-win](https://thefab20s.com/wp-content/uploads/2021/03/win-win-win-michael-scott-the-office.gif) situation. GitHub wins by increasing its network effect, GitHub's users win by learning about interesting projects, and project creators win by getting exposure to their work (the feed will even highlight users and projects seeking sponsorship).

Something else to note about this feed is that I haven't found that it rapidly changes or provides value in checking it everyday. In fact, over the course of writing this post (a few days) my generated recommendations have been the same.

Rather than being something you need to constanly check, I find that the feed lends itself well to [Cal Newport's advice](https://www.calnewport.com/blog/2017/10/02/are-you-using-social-media-or-being-used-by-it/) of limiting social media interactions to a fixed schedule. I doubt you need to get as formal as scheduling time to check this feed, but you would probably get all the value you need if you looked at it for a few minutes once a week.

## Conclusion

These changes are still early days for me. I haven't actually revisited projects I've starred yet, so I don't know if that problem is completely solved. It's also possible I go overboard with starring and following, and the feed becomes useless. Caveats aside, I _feel_ like I am getting value out of these changes, so, while that feeling continues, I endorse others trying it for themselves.

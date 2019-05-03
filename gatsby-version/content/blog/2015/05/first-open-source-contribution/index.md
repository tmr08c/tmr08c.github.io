---
title:  "My First Open Source Contribution"
date:   2015-05-03 19:29:02
categories: ['opensource']
---

In working towards my [original goals](/2015/02/why-i-made-this-blog/), I finally opened my [first pull request](https://github.com/bbatsov/rubocop/pull/1857) on an open source project.

For my first project I chose [Rubocop](https://github.com/bbatsov/rubocop), a gem that analyzes Ruby code to check for compliance with the [Ruby style guide](https://github.com/bbatsov/ruby-style-guide). This is a tool that we use multiple times a day at work - no code gets merged in until Rubocop is happy with it.

## Why

The fact that we use Rubocop at work factored into why I chose to contribute to this gem, but the majority of the credit goes to a friend.

I have been sharing my programming goals with this friend to force him into being my accountability partner. For example, because he knows I want to blog, now when we talk about something interesting the conversation inevitably leads to, "this could be a good blog post". It's really nice having this external force pushing you to stay on track.

It is also helpful when your accountability partner has similar goals because it adds a bit of the competitive spirit to the mix. It's one thing to have someone that reminds you to run and a whole other when they are trying to outpace you every time you run together.

My friend metaphorically outpaced me by [opening a pull request](https://github.com/bbatsov/rubocop/pull/1818) for Rubocop and telling me about it after it was merged. This was a great reminder of the fact I have wanted to contribute to open source and still hadn't. This also proved advantageous in the fact that discussing some of the issues he faced lowered my entry barrier a bit.

## What I Learned

### New Code Bases are New

The [Pragmatic Programmer](https://pragprog.com/book/tpp/the-pragmatic-programmer) is often cited for the fact is says that [you should learn a new language every year](http://blog.teamtreehouse.com/learn-a-new-programming-language-every-year). I think it can also be beneficial to learn new code bases as well. This is a slightly easier way to get out of your comfort zone and can still provide similar results.

One of the reasons I want to contribute to open source is to widen my exposure to (preferably good) code. It is hard to know if the programming paradigms you practice are the best when they are all you are familiar with. Even if you learn new design patterns they may not fit your current project which will cause you to forget about them when you need them; or worse - you use them even though they are a poor fit because you aren't familiar enough to know.

One of the reasons design patterns exist is to make the code easy to understand and build upon. When you begin work on a new project you are in the best position to see how clear and easy it is to add features or fix bugs. Can you figure out what you need to change? Do you know where this change should go or are you forced into [grep driven development](http://stevenharman.net/bag-of-methods-module-and-grep-driven-development)?

Take notice of the ease of development and what the maintainers do to make it that way.

I found it a bit overwhelming being in a new code base again, but it was designed in such a way where I was able to find what I was looking for and add the needed functionality with ease.

### Choose the Right Thing&copy;

For each rule in the style guide, Rubocop has a Cop class that checks if this rule has been violated. Some of the rules have clear, correct alternatives, and for some these "clear alternative" rules, there are Cops with the functionality to rewrite the offending code to use the "correct" style. Some of these cops do not have this [auto-correct](https://github.com/bbatsov/rubocop/wiki/Automatic-Corrections) functionality built in yet - and herein comes the contribution opportunity.

My friend's pull request dealt with adding auto-correct functionality to one of Rubocop's Cops objects. Based on my friend's success I chose to add additional auto-correct functionality as well. This was a great area to start since the logic for detecting the problem was already in place and the style guide tells you what the auto-correct should do, lending itself nicely to TDD.

### Sometimes it's Fun to Struggle

The auto-correct functionality I made the pull request was not actually the first issue I tried to resolve. I spent a few hours the night before attempting to add auto-correct functionality to remove Windows' carriage return characters from source files. I even had a decent solution but found an edge case I was concerned with handling properly. This inability to quickly solve the problem was actually kind of fun. It felt like a late night in college trying to get an assignment in on time. I drank Red Bull, I played loud music, and I randomly stood up and just started dancing to get the blood flowing.

#### A Brief Aside on Why College Life Seems Exciting

I was offered my job after a summer internship between my first and second year of my Master's program. Rather than go back and finish my degree, I decided I wanted to continue working.

 I favored work life over school life - I liked the fact that my code was to do something more than just to get a grade and I liked the attachment to a code base since it stuck around for longer than just to turn it in. I also really liked the more structured lifestyle. I remember during my internship I would get off work and not know what to do with myself, in a good way. I felt carefree.

I now realize that I too quickly grew complacent with this lifestyle. Working on open source reignited some passion in me where I wanted to stay up late working on code, where I wanted to learn this new code base and contribute to it.

#### The Struggle is Real...fun

Getting out of the comfort zone of the same code base and the same working hours was a great way to remind me why I love the industry I am in. One of the best parts of this industry is how easy it is to work outside of work. I can do side projects, lean a new language, contribute to open source, write blog posts, or any number of other activities to stretch my brain and I don't **need** any thing other than my computer to do it.

### Rejection is Good

My first solution was rejected and I am happy it went that way.

When working to auto-correct the code you are passed in an object that hold the code you want to "rewrite". Being unfamiliar with the object being passed in I looked at other solution and played around in debugger to familiarize myself. I then stumbled on one solution where rather than using the passed in object's methods they just used regular expressions to rewrite the code. This seemed way easier since I was already familiar with this concept. I got my tests passing and nervously created a pull request.

I was given feedback that it is advisable to use node manipulation over regular expressions. That is, I should have properly used the object passed in.

Admittedly this stung a bit at first - I mean, my friend got his pull request merged in first try, why couldn't I? Then I realized that was my Ego talking. I wasn't doing this to get more nerd points then my friend, I was doing this to learn and to help a project that I use and believe in; a project that, if it accepted my code that was more error prone, could be less likely to survive.

#### Rejecting for the Good of the Code

Rejecting pull requests means that the project is cared for. Open source projects need a strong maintainer that is willing to say no for the health of the project.

#### Learning from Rejection

By rejecting the solution I chose, the "easy" solution, I got to go back and actually learn something new. This was the main reason I wanted to contribute to open source in the first place. This rejection forced me to do what I came to do, get my hands dirty and learn something new. This something new will also be beneficial in further contributing to the Rubocop gem.

#### Rejecting My Ego

I mentioned my Ego before. I have been reading [The Rock Warrior's Way](http://www.amazon.com/The-Rock-Warriors-Way-Training/dp/0974011215) and one of the important aspects of the book is realizing how your Ego affects you in all aspects of your life.

The Ego doesn't care about long term success. The Ego is like a drug addict, constantly searching for those quick highs. The Ego is also what gets hurt when you "fail". The Ego doesn't care about the journey or the learning; even though the journey and the learning are what get you to be better.

We need to learn to silence our Egos. It's not about being the best or getting more nerd points than someone else, it's about making yourself better at your craft.

I recently listened to a discussion about how children learn versus adults. One of my big take aways is that adults quickly get discouraged when they don't master something. Seeing that I didn't get it right on my first try was a blow to my Ego and almost made me want to not try any more, I didn't want to hurt my Ego again. This is ridiculous though.

The problem is, this happens in all aspects of life. Before following Couch to 5K I tried and quit running because my first attempt to run as much as everyone else right off the bat ended in me being tired and sore. People say a new programming language isn't for them because they didn't become as effective in it as their main language after trying it out one weekend.

We need to silence our Ego and **embrace the fail**. Enjoy the journey. Enjoy learning something new. Enjoy the beginning of your [10,000 hour journey to a new mastery](http://www.wisdomgroup.com/blog/10000-hours-of-practice/).

## Conclusion

I am happy I finally made the first step towards becoming an open source contributor. I hope to contribute more now. I want to fix the first auto-correct functionality I started and maybe work on some new ones as well. I would also like to write a Cop for one of the style guide rules not yet checked by Rubocop, I think this would be a good "next level" task to work on.

Since part of the point of learning through open source is looking at different code bases I plan to stalk [Code Triage](http://www.codetriage.com/) for additional contribution opportunities so I can expand beyond Rubocop as well. What projects are you contributing to? Any tips on finding good introductory features to work on?

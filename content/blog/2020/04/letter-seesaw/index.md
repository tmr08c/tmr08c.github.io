---
title:  "Letter Seesaw"
date:   "2020-04-18T20:02:34.781Z"
categories: ["soft skills"]
---


My friend and former co-worker [@C-Saunders](https://github.com/c-saunders) and I make a point to periodically catch up with each other. Because I am lucky and enjoy the industry I work in, our discussions about work are a personal highlight. 

During a recent conversation, we discussed decision making at work. We ended up discussing two different "spectrums" that could impact your decision making. One of these came from his new boss and the other is something he came up with during our conversation. These ideas stuck out to me because, for both of these spectrums, we had [tautograms](https://en.wikipedia.org/wiki/Tautogram) (word groups that bein with the same letter). I thought they be a useful thought technology for self-evaluation. As a bonus, by starting with the same letters, these word pairings are easier to remember. This increases the chances that I remember and use them. 

 The two spectrums we discussed were: 

* Balancing being **pragmatic** and being **prudent**
* Knowing when it's time to **struggle** and when it's time to **solve**

Depending on the situation, my position on these spectrums will vary. Without acknowledging my shifting position, I can fall into common traps during the decision making process. Having the idea of these spectrums can help me take a step back and catch myself. 

## Pragmatic versus Prudent

* [**Pragmatic**](https://www.lexico.com/en/definition/pragmatic) - dealing with things sensibly and realistically in a way that is based on practical rather than theoretical considerations.
* [**Prudent**](https://www.lexico.com/en/definition/prudent) - acting with or showing care and thought for the future.

Everyone in the software industry has dealt with trying to balance these two ends of the spectrum. We want to build solid, reliable systems that scale, but we also want to avoid [premature optimization](https://wiki.c2.com/?PrematureOptimization) and writing code for things [we aren't going to need](https://martinfowler.com/bliki/Yagni.html). 

When dealing with day-to-day work and planning I try to keep the idea of [perfect being the enemy of good](https://en.wikipedia.org/wiki/Perfect_is_the_enemy_of_good) in mind, but, in the moment, it's _so_ easy to forget. 

### Planning

I have found the planning part of my work to be where I most often fall into a mindset of being prudent over pragmatic. I'm not going as far as a [big upfront design](https://wiki.c2.com/?BigDesignUpFront), but I do try to think out how functionality should work now and _could_ need to work in the future. 

This "could work like" thinking can be dangerous, because I cannot predict the future. In a recent example, I was tasked with collecting a few specifics metrics about a process someone completes (e.g., who submitted this request, when they submitted it, etc.). We had a list of specific asks from the business, so things should have been straightforward. However, when I began sketching out what I wanted my schema to look like, I started thinking of other information the team _could_ want to ask about our data. My previous experience of working with [Google Analytics](https://analytics.google.com/analytics/web/) biases me to think we would eventually want to track and report on "all the things," and I started thinking of how to model a generic "event" for reporting purposes. 

Fortunately, I was able to leverage stand up the next day to signal to the team the rabbit hole I began finding myself digging down. This led to scheduling some time with a coworker to bring me into reality - we don't know _everything_, but we know _something_; we know what the business side already wants to report on. Getting started with the current request allows us to get to "good" and talk about "perfect" along the way. Maybe in a few months, we will wish we had a generic "event" model. Even if this is true, we will have delivered months of _some_ value in the interim, **and** will know more about what we really want and (more importantly) _need_ out of our reporting. 

### Performing 

I have found that when it comes to hands-on-keyboard, code-writing time, it can be easier to be more pragmatic. 

I think some of my pragmatism with coding comes out of laziness. Asking "do we _really_ needs?" this seems to come up a lot more naturally when I have to actually _do_ something. Fortunately, [some consider this to be a  virtue](http://threevirtues.com/).

I've also found that some industry practices lend themselves to guide me towards making pragmatic decisions during the coding phase. One great example of this is the idea of [red-green-refactor](https://www.jamesshore.com/Blog/Red-Green-Refactor.html). When disciplined in this approach, you will maintain a focus on doing "just enough." You write just enough test to fail, and then just enough logic to make that test pass. Every once in a while, you change things just enough to make the codebase easier to work with. 


### Being Agile

Looking at the [Agile Manifesto](
https://agilemanifesto.org/), I think it is a guide to pragmatism. For example, one of the points is to favor responding to change over following a plan. I believe this is in response to some of the upfront requirements and design work that came out of the [waterfall method](https://en.wikipedia.org/wiki/Waterfall_model). It feels _prudent_ to spend a bunch of time before you write any code trying to map out the whole project. However, in practice, you can't see the future and you can't plan for everything, sp projects inevitably go off-plan. In practice, it is more _sensible_ to expect change and be ready to respond to. 

While I think the agile manifesto falls on the side of pragmatism, there is a meta-level of pragmatism that I think people don't always acknowledge:

> That is, while there is value in the items on the right, 
> we value the items on the left more

A part of the wisdom and pragmatism of the Agile Manifesto is that it acknowledges it's not all one way. 

### Takeaway

There is a _balance_ to things. This spectrum idea may start to break down when you consider that it may be _pragmatic_ to be _prudent_ at a certain point in time, but the point isn't about assigning a number on the pragmatic-prudent scale. The idea of this spectrum is to have a tool for thinking about your [biases](https://en.wikipedia.org/wiki/List_of_cognitive_biases) and using that self-awareness to make a better decision. 

Maybe our industry (or at least people that include the authors of a book titled [_The Pragmatic Programmer_](https://pragprog.com/book/tpp20/the-pragmatic-programmer-20th-anniversary-edition)) lean towards pragmatism, but it's important to acknowledge the balance and be mindful of where you are in comparison to where you want to be. 

## Struggling versus Solving

This one is almost a more specific instance of pragmatic versus prudent, but I was so excited we had another tautogram that it inspired me to start writing. We were talking about how to balance spending the time struggling to solve a problem and learning on your own, and getting to a solution as quickly as possible. 

In a [recent post](/2020/04/how-i-debug-my-dependencies#asking-for-help) I mentioned how I can struggle with asking for help, especially from open-source strangers. This struggle versus solving discussion partially came out of me trying to self-reflect on how I could improve. How much time should I spend trying to debug an issue with a gem and learning how it works before I should ask for help solving my problem.

While the above example uses asking a library author for help, that isn't the most common scenario. In fact, the more common example of this is asking your co-workers for help.

Personally, I tend to fall on the side of struggling. I think there is [value in taking some time to struggle](https://news.stanford.edu/2019/09/30/embrace-struggle-education-professor-challenges-common-beliefs-teaching-learning/) with a problem to better understand the eventual solution. In addition to getting practice trying to learn something, I think it helps the lessons better stick with me. However, I _know_ I have some opportunity to better balance where I fall on this spectrum.

Being a new team member has put this balance to the test for me. Working on a new, large codebase has resulted in numerous "how do I..." and "where is..." moments. For many of these, I have silently struggled - grepping across multiple repositories or jumping between Confluence and GitHub. For many of these problems, I could have pinged someone and probably would have had an answer to my question almost immediately. Fortunately, most of these problems haven't taken up too much time, but it can be a slippery slope; especially when not asking for help _now_ makes it [harder for me to ask in the future](https://www.andymort.com/hard-asking-for-help/).

If I am hitting a bug, do I try to get help after an hour? Four hours? A day?  At what point would it be irresponsible to myself and employer to get help and lose out on growing my skillset? At what am I being irresponsible to my employer for not getting assistance and just solving the problem? 

Maybe the question to keep in mind is: "have I struggled enough where if I get the answer given to me, it will stick?" This can help prevent you from spinning your wheels with no results while continuing to evolve your problem-solving skills. Another good question to keep in mind is one you should ask the person that helped you solve the problem: "how did you figure this out (so I can figure it out for myself next time)?"

## Conclusion

Unfortunately, this post isn't the kind that _solves_ the "problem" of being too extreme to one side or the other. I also don't expect it to reveal anything new to people; [life is about balance and moderation](https://xkcd.com/1592/) and software is full of ["it depends" answers](https://softwareengineering.meta.stackexchange.com/questions/766/how-to-answer-it-depends-questions). 

However, I think having these terms can provide a tool for observing your behavior or the behavior of those you work with. Maybe the next time you dealing with a difficult problem at work you can take a step back and assess where you fall on the struggle-solve spectrum, and decide what to do next. _Maybe_ you could even work with your manager to see where they think you fall on these spectrums and where they see opportunities for you to grow. 

### Actionalable Suggestions

On top of inspiring this post [@C-Saunders](https://github.com/c-saunders) read an early draft and suggested including some actionable suggestions around this topic. As I saw in the section above, I had resigned myself to saying "yep, this is a problem...that's all I got." However, I do agree it would be nice to have some suggestions for how to help yourself avoid going too far to one end of either of the spectrums.

### Timeboxing 

When sharing the feedback, Charlie mentioned the idea of setting time limits. [Timeboxing](https://hbr.org/2018/12/how-timeboxing-works-and-why-it-will-make-you-more-productive) is a great strategy for an individual and a team. 

At the team level, time-boxing can help reveal the ["appetite"](https://basecamp.com/shapeup/1.2-chapter-03#setting-the-appetite) the team has for some amount of work. How much time does the team think this is worth. If everyone estimates it will take two weeks to solve this problem, does that feel like too much to the team? 

While there are a number of potential negative consequences of deadlines, one way I've seen them provide value is in helping to force prioritization. If your team has decided they want to [spike](http://www.extremeprogramming.org/rules/spike.html) on a few potential options for a new feature, they may also decide they want to timebox it to a few days, let's say three. Having three days can help force some pragmatism. Rather than 

---
Charlie suggested the idea of setting time-limits or breakpoints for yourself. This can be especially useful in the day-to-day balance of struggle versus solve. If you think a bug should take you two hours, may set a timer or calendar appointment. If you are still working after your estimated time, ask your self some questions:

Do I know what the problem is now?
Do I know how I can solve the problem?
Do I know what I'm going to do next?
What is my new estimate?
Would someone else be able to help me get this done sooner?
How long would it take to get someone else up to speed? 

You can even use existing breaks in work such as lunch or a meeting as a chance to check-in with yourself. Using these stopping points provides an additional benefit that your co-workers are also stopping at the same time, so switching gears to help you after the meeting may be easier for them.


If you are timeboxing looking into a bug to two days, the amount of time you should struggle versus getting to a solution is a bit more clear. 

Other ideas
Pomodoro (timeboxing)
Breaks? 
Questions mentioned in the post 



----
Follow-up posts

## Where to focus

Another area we may struggle with this is deciding where to focus our attention. It makes senes to learn more about the current language or framework your using, but [maybe it's dying](https://www.reddit.com/r/learnprogramming/comments/7oam8p/which_programming_language_is_dying/). We want to learn new things to grow and be future-proof, but [maybe the hype _isn't_ real?](https://blog.daftcode.pl/hype-driven-development-3469fc2e9b22) (ignore what that article says about Elixir and Phoenix, they **are** the real deal 😉).

I believe this, "where should I focus my energy for the future?", thinking is where I struggle the most.  I have multiple lists of things I want to learn more about spread across various note-taking and to-do list apps. I greatly struggle with not wanting to miss out on the next big thing, or having my skills become obsolete. Unfortunately, this overthinking and over worrying leads to [analysis paralysis](https://xkcd.com/1801/) and inaction. At the time of writing, I have tabs open about the new [Redwood](https://github.com/redwoodjs/redwood) JavaScript framework and [ReasonML](https://reasonml.github.io/) (trying to be prudent and track upcoming trends), and I had originally planned to spend the day looking into [concurrent-ruby](https://github.com/ruby-concurrency/concurrent-ruby) to sharpen my Ruby skills for my new job (the pragmatic choice). Instead of any of those, I'm starting a draft of this post.

## Trust and honesty with co-workers

This conversation kicked off because Charlie draft up a plan for a more complicated solution, a co-worker helped push to a "good enough." This same thing happened to me after I started writing this post.




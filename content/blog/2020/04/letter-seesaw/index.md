j---
title:  "Letter Seesaw"
date:   "2020-04-18T20:02:34.781Z"
categories: ["soft skills"]
---

My friend and former co-worker [@C-Saunders](https://github.com/c-saunders) and I make a point to periodically catch up with each other. Because I am extremely fortunate and enjoy the industry I work in, one of my favorite parts of our conversations is discussing work. We share a similar professional background in that the bulk of our careers thus far have been with the same company (for very similar amounts of time) and we both recently began working at new companies. This shared background provides a solid starting point for comparing and contrasting how our new teams work and what practices we can pick up from one another. I've really appreciated these sessions and have found them to be a great way to keep up with a friend and also get more exposure to the software development world. 

During our most recent conversation, we discussed two different spectrums that can impact decision making at work. One of these came from my co-worker's new boss and the other is something he came up with. These ideas stuck out to me because, for both of these spectrums, we had words that began with the same letter. While not as extreme as other examples on Wikipedia, I _believe_ we can consider these both to be [tautograms](https://en.wikipedia.org/wiki/Tautogram). When we hit our seconds tautogram,  I felt the urge to write it and our previous one down. I felt these word pairings would be useful thought technology for self-evaluation.

 The two spectrums we discussed were: 

* Balancing being **pragmatic** and being **prudent**
* Knowing when it's time to **struggle** and when it's time to **solve**

These are both examples of spectrums on which I am regularly shifting my position; often without actively thinking about where I currently stand. There may be value in acknowledging where I fall on these spectrums as I believe they play a major role in **most** of the decisions I make at work. 

## Pragmatic versus Prudent

* [**Pragmatic**](https://www.lexico.com/en/definition/pragmatic) - dealing with things sensibly and realistically in a way that is based on practical rather than theoretical considerations.
* [**Prudent**](https://www.lexico.com/en/definition/prudent) - acting with or showing care and thought for the future.

I believe most people in the software industry are familiar with trying to balance these two ends of the spectrum, especially in day-to-day feature development. We want to build solid, reliable systems that scale, but we also want to avoid [premature optimization](https://wiki.c2.com/?PrematureOptimization) and writing code for things [we aren't going to need](https://martinfowler.com/bliki/Yagni.html). I try to keep the idea of [perfect being the enemy of good](https://en.wikipedia.org/wiki/Perfect_is_the_enemy_of_good) in mind, but, in the moment, it's _so_ easy to forget. 

Another area we may struggle with this is deciding where to focus our attention. It makes senes to learn more about the current language or framework your using, but [maybe it's dying](https://www.reddit.com/r/learnprogramming/comments/7oam8p/which_programming_language_is_dying/). We want to learn new things to grow and be future-proof, but [maybe the hype _isn't_ real?](https://blog.daftcode.pl/hype-driven-development-3469fc2e9b22) (ignore what that article says about Elixir and Phoenix, they **are** the real deal 😉).

I believe this, "where should I focus my energy for the future?", thinking is where I struggle the most.  I have multiple lists of things I want to learn more about spread across various note-taking and to-do list apps. I greatly struggle with not wanting to miss out on the next big thing, or having my skills become obsolete. Unfortunately, this overthinking and over worrying leads to [analysis paralysis](https://xkcd.com/1801/) and inaction. At the time of writing, I have tabs open about the new [Redwood](https://github.com/redwoodjs/redwood) JavaScript framework and [ReasonML](https://reasonml.github.io/) (trying to be prudent and track upcoming trends), and I had originally planned to spend the day looking into [concurrent-ruby](https://github.com/ruby-concurrency/concurrent-ruby) to sharpen my Ruby skills for my new job (the pragmatic choice). Instead of any of those, I'm starting a draft of this post.

Looking at the [agile manifesto](
https://agilemanifesto.org/), I think it is a guide to pragmatism. For example, one of the points is to favor responding to change over following a plan. I believe this is in response to some of the upfront requirements and design work that came out of the [waterfall method](https://en.wikipedia.org/wiki/Waterfall_model). It feels _prudent_ to spend a bunch of time before you write any code trying to map out the whole project. However, in practice, you can't see the future and you can't plan for everything, sp projects inevitably go off-plan. In practice, it is more _sensible_ to expect change and be ready to respond to. 

While I think the agile manifesto falls on the side of pragmatism, there is a meta-level of pragmatism that I think people don't always acknowledge:

> That is, while there is value in the items on the right, 
> we value the items on the left more

There is a _balance_ to things. This spectrum idea may start to break down when you consider that it may be _pragmatic_ to be _prudent_ at a certain point in time, but the point isn't about assigning a number on the pragmatic-prudent scale. The idea of this spectrum is to have a tool for thinking about your [biases](https://en.wikipedia.org/wiki/List_of_cognitive_biases) and using that self-awareness to make a better decision. 

Maybe our industry (or at least people that include the authors of a book titled [_The Pragmatic Programmer_](https://pragprog.com/book/tpp20/the-pragmatic-programmer-20th-anniversary-edition)) lean towards pragmatism, but it's important to acknowledge the balance and be mindful of where you are in comparison to where you want to be. 

## Struggling versus Solving

This one is almost a most specific instance of pragmatic versus prudent, but I was so excited we had another tautogram (also excited to have been able to Google my way to that word) and represented two ends of a spectrum I have to make it a thing.

We were talking about how to balance spending the time struggling with a problem and learning on your own with getting to a solution. In a [recent post](/2020/04/how-i-debug-my-dependencies#asking-for-help) I mentioned how I can struggle with asking for help, especially from open-source strangers, and this struggle versus solving discussion partially came out of me trying to self-reflect on how I could improve. However, asking for help isn't limited to reaching out to a library author. In fact, the more common example of this is asking your co-workers for help.

Personally, I tend to fall on the side of struggling. I think there is [value in taking some time to struggle](https://news.stanford.edu/2019/09/30/embrace-struggle-education-professor-challenges-common-beliefs-teaching-learning/) with a problem to better understand. In addition to getting practice trying to learn something, I think it helps the lessons better stick with me. However, I _know_ I have some opportunity to better balance where I fall on this spectrum.

Being a new team member has put this balance to the test for me. Working on a new, large codebase has resulted in numerous "how do I..." and "where is..." moments. For many of these, I have silently struggled - grepping across multiple repositories or jumping between Confluence and GitHub. For many of these problems, I could have pinged someone and probably would have had an answer to my question almost immediately. Fortunately, most of these problems haven't taken up too much time, but it can be a slippery slope; especially when not asking for help _now_ makes it [harder for me to ask in the future](https://www.andymort.com/hard-asking-for-help/).

If I am hitting a bug, do I try to get help after an hour? Four hours? A day?  At what point would it be irresponsible to myself and employer to get help and lose out on growing my skillset? At what am I being irresponsible to my employer for not getting assistance and just solving the problem? 

Maybe the question to keep in mind is: "have I struggled enough where if I get the answer given to me, it will stick?" This can help prevent you from spinning your wheels with no results while continuing to evolve your problem-solving skills. Another good question to keep in mind is one you should ask the person that helped you solve the problem: "how did you figure this out (so I can figure it out for myself next time)?"

## Conclusion

Unfortunately, this post isn't the kind that _solves_ the "problem" of being too extreme to one side or the other. I also don't expect it to reveal anything new to people; [life is about balance and moderation](https://xkcd.com/1592/) and software is full of ["it depends" answers](https://softwareengineering.meta.stackexchange.com/questions/766/how-to-answer-it-depends-questions). 

However, I think having these terms can provide a tool for observing your behavior or the behavior of those you work with. Maybe the next time you dealing with a difficult problem at work you can take a step back and assess where you fall on the struggle-solve spectrum, and decide what to do next. _Maybe_ you could even work with your manager to see where they think you fall on these spectrums and where they see opportunities for you to grow. 


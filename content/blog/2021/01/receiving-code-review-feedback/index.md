---
title: 'Receiving Code Review Feedback'
date: '2021-01-20T06:14:13.265Z'
categories: ['code review', 'soft skills']
---

Code reviews are a common topic among blog posts; this is for a good reason. Code reviews are a powerful tool for spreading knowledge, improving code quality, and, when done right, can strengthen team bonds. Often, content around code reviews focuses on how to _give_ better code reviews. In this post, I want to cover how to be on the _receiving_ end.

If you are interested in content on giving code reviews, I recommend the talk [_Implementing a Strong Code-Review Culture_](https://youtu.be/PJjmw9TRB7s) by [Derek Prior](https://twitter.com/derekprior?lang=en). It provides a comprehensive introduction to code reviews with a strong focus on conducting a code review that can be well received.

## You are not your code

A blog post about being on the receiving end of a code review may seem superfluous. All you have to do is put the code up and wait for the review, right? Not when our sneaky friend, ego, gets involved.

As [knowledge workers](https://en.wikipedia.org/wiki/Knowledge_worker), our self-worth can feel closely tied to the knowledge we bring to our team. If tied too closely, a lack of knowledge can feel like a lack of worth. For developers, this knowledge often comes in the form of code we deliver. One way this relationship between perceived knowledge and perceived self-worth reveals itself is in our response to receiving code review feedback. If you receive critiques on your code during a review, it may trigger your ego to feel "under attack" for a perceived lack of knowledge. In turn, this can potentially result in feelings of being less valuable to the team. 

Even on a team with a strong code review culture that is considerate of each other's feelings, code reviews are still providing someone with (often critical) feedback. If you have your ego tied into the code you wrote, even a well-intentioned code review can feel like a personal attack. If they don't like the way you wrote this line of code...do they think you don't deserve to be on the team? While this jump from a comment on a line of code to worrying about job security can feel over-the-top, I know from experience it happens.

 Instead, remember, **you are not your code**. 

While much easier said than done, I have a few practices that can help make it easier to be less emotionally attached to the code you have up for review and more receptive to feedback.

## Preparing for Reviews

### Step away

I find the advice of stepping away (or taking a step back) to be useful in all walks of life, and handling code review feedback is no exception.

When you receive feedback on something you just worked on, it is more difficult to process it objectively than something you worked on days or even hours ago. This difficulty comes from the emotional attachment you form as you put effort into your work. Taking some time to "step away" from the code you just worked on can help you begin to detach these emotions and be more prepared to receive feedback.

Stepping away can be a simple context switch like processing your emails after you've pushed your code. However, it is more powerful to have physical separation. As silly as it may sound, I have found that stepping away from my computer helps me feel less attached to my code. Physical separation can be something like refilling your drink or stepping outside for some fresh air. Whatever you choose, it doesn't have to take a lot of time but should provide some physical and temporal distance between yourself and the code for which you are soliciting feedback. 

### Review your code first

The suggestion to review your code first may sound like a form of [self-deprecation](https://en.wikipedia.org/wiki/Self-deprecation) - attempting to call out yourself before anyone else can. However, the intention is not to beat others to the punch. Instead, you want to review your changes to **change your relationship with your code**.  You should aim to review your code just like you would anyone else's.  To do this, you want to shift from the creator's mindset to the reviewer's mindset. 

The creator's mindset is protective of the code and attached to the choices made. It went through the difficult journey of getting to the solution and may have a hard time seeing other options. 

On the other hand, the reviewer's mindset lacks the baggage of the journey. It comes to the changes open for anything. This openness can lead to seeing improvement opportunities that you become blind to as the creator.

This mindset transition is difficult. One practice that can help is to start reviewing other people's code first. Transitioning into the reviewer's mindset for code you didn't write is significantly easier than for code you did. Once you are in the reviewer's mindset, it becomes easier to maintain it, even when shifting back to code that you _did_ write.

When you come back to review your code, you want to keep your environment the same as when you were reviewing the other code changes. My reviewer setup includes having a browser window with the pull request taking up the majority of the screen.  I also have a terminal for diving deeper into the code or testing out suggestions I may have. This setup is different than what I would have when writing code; the visual difference provides a cue to my brain that we are (still) in review mode. 

Even if the owner's mindset sticks around, it is still valuable to review your code. You may not find drastically different approaches to take, but you can often find the low hanging fruit fixes (spelling, style, etc.). You can also use the review as an opportunity to annotate your pull request and make reviewing easier for your teammates by explaining critical aspects of your changes or calling our areas of concern. 

### Start new work

Depending on your team's mindset around [WIP limits](https://www.planview.com/resources/articles/benefits-wip-limits/), you could also start new work. When you start working on a new problem, your emotional investment shifts away from your old work and onto this new problem. As mentioned in the stepping away section, this emotional detachment to your old work may make it easier to receive critical feedback. 

However, from a project management point of view, this detachment can cause productivity issues. You can end up feeling like the feedback on your old code is just a distraction from the current work you are doing. If the detachment goes too far, it can lead to delays in getting code shipped to users.

With these pros and cons in mind, consider using this method more sparingly than other options.

## Handling the Review

At this point, we've prepared ourselves to be emotionally ready for code review feedback. Once we start to receive feedback, we want to continue managing our ego as we discuss possible changes with our reviewers. 

### Step away (again)

Above, we discussed stepping away before requesting a code review. The goal was to become more receptive to feedback by creating distance between yourself and the code you are putting up for review. We may also want to step away _after_ we have received code review feedback. Sometimes, even if we've done our best to prepare, we will still struggle with receiving critiques on our code.

When preparing to request feedback, we were trying to separate ourselves from the code. We almost wanted to forget that we wrote the code so we would receive any feedback more objectively. 

Once we've received feedback, we want to handle things a bit differently. We still want to have separation from our ego, but we do not want to become so removed we discard the feedback altogether. Instead, we want to take the time to understand it. In this case, I think it makes even more sense to physically step away instead of moving on to a different task. Physical activity can help settle your emotions while still letting you focus on and process the feedback. 

I am a proponent of physical separation because it helps me think less about the actual work and code needed to implement a suggestion. Instead, I can think about alternative solutions at a high level - focusing more generally on how they would work rather than what I would need to do to implement them. Because it's easier for me to _imagine_ possible code changes than it is actually _write_ the code, I am less likely to avoid this process and am more willing to explore the suggestions offered to me. 

If stepping away and taking a walk doesn't do the same thing for you, try to find what does (maybe give [hammocks](https://www.youtube.com/watch?v=f84n5oFoZBc) a try). Find a practice that helps you separate yourself from the actual work involved with updating code and get into a headspace of curiosity and exploration. 

### Objectively Analyze

Depending on the situation, it may or may not make sense to invest in changes suggested during a code review. The determining factor of whether it makes sense is _not_ how we feel that day, though.

When analyzing the feedback, ask yourself questions like: do we have a deadline, is either solution objectively better for our needs (speed, memory usage, etc.), and does going forward with the current solution make it harder to use the suggested solution in the future? The answers to these questions provide more objective points of discussion when comparing alternative solutions.

After processing review feedback, you may find advantages or disadvantages to either solution. Taking the time to think about the options provides you with the tools to have an objective and constructive conversation with your reviewer and avoid relying on emotional arguments. A result of these more productive conversations is a stronger codebase and more trust within the team.

## Conclusion

Code reviews are an essential yet challenging part of the software development lifecycle. Part of this challenge comes from the first-line response your ego may have to feedback on your code. 

Working to separate yourself from the code you write can help reduce some of the emotional aspects of this process and make it easier to embrace the value that code reviews provide. With time, space, and practice, this separation will come more naturally. 

While you likely won't need to take a walk after every piece of code review feedback, it's helpful to have these practices in your toolbelt for the times you find your emotions are running high.

And, when in doubt, assume best intentions (and maybe step away ;)). 


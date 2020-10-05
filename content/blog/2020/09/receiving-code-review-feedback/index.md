---
title: 'Receiving Code Review Feedback'
date: '2020-09-11T06:14:13.265Z'
categories: ['code review', 'soft skills']
---

Code reviews are a common topic among blog posts and videos; this is for a good reason - code reviews are a powerful tool for spreading knowledge, improving code quality, and, when done right, can strengthen team bonds.

Often, content around code reviews focuses on how to _give_ better code reviews. In this post, I want to cover how to be on the _receiving_ end.

If you are interested in content on giving code reviews, I recommend the talk [_Implementing a Strong Code-Review Culture_](https://youtu.be/PJjmw9TRB7s) by Derek Prior. It is a great overall introduction to code reviews with a strong focus on how to give a code review that can be well received. It has been hugely impactful on how I give code reviews.

## You are not your code

A blog post about being on the receiving end of a code review may seem superfluous; all you have to do is put the code up and wait for the review, right? Not when our sneaky friend, ego, gets involved.

As "knowledge workers," our self-worth can feel closely tied to the knowledge we bring to our team. If tied too closely, a lack of knowledge can feel like a lack of worth. For developers, this knowledge often comes in the form of code we deliver. When you attach your worth to your code, every piece of feedback and discussion is an attack on the ego and your perceived worth.

Even on a team with a strong code review culture that is considerate of each other's feelings, code reviews are still providing someone with (often critical) feedback. If you have your ego tied into the code you wrote, even a well-intentioned code review can feel like a personal attack. If they don't like the way you wrote this line of code...do they think you don't deserve to be on the team? While this jump from a comment on a line of code to worrying about job security can feel over-the-top, I know from experience it happens.

 Instead, remember, **you are not your code**. 

While much easier said than done, I have a few practices that can help make it easier to be less emotionally attached to the code you have up for review and more receptive to receiving feedback.

### Step away

Stepping away is advice that can be given in all sorts of situations, including preparing for code review feedback. When you receive feedback on something you just worked on, it is more difficult to receive it objectively than something you worked on days or even hours ago.

Stepping away can be as simple as checking chat notifications after you've pushed your code. However, it is more powerful to have physical separation. If you can, take a break from screen time. This can be something quick like refilling your drink or longer like taking a walk. You want to create space between yourself and the code you are soliciting feedback on. As silly as it may sound, I have found that actual, physical space helps me feel less attached to my code. If you're skeptical, try it once. Next time your code is ready for a review, go for a walk. I think when you come back, you will see your code through a new lens.

This process of stepping away shouldn't even block reviews from getting done. If your team primarily does asynchronous reviews, you can push your code, request reviews, and step away. If your team does synchronous reviews, you can find and schedule a future time that works for the reviewer before taking your break.

### Review your code first

This may sound similar to the concept of self-deprecating humor - make fun of yourself before anyone else can. However, the intention is not to beat others to the punch, but instead to **change your relationship with your code**.  You want to shift from the creator's mindset to the reviewer's mindset to change how you are thinking about the code you wrote.

The creator's mindset is protective of the code and attached to the choices made. It went through the difficult journey of getting to the solution and may have a hard time seeing other options. 

On the other hand, the reviewer's mindset lacks the baggage of the journey. It comes in open for anything. This openness can lead to seeing improvement opportunities that you become blind to as the creator.

You should review your code just like you would anyone else's. By getting into the same "mode" you would when reviewing a teammate's code, you trigger your brain to shift mindsets.  

One of the easiest ways to shift mindsets is to start reviewing other people's code. Rather than going straight into reviewing your own code first, review any open pull requests your team may have. Shifting into the reviewer's mindset for code you didn't write is significantly easier. Then, once you are in the reviewer's mindset, it becomes easier to maintain it, even when shifting to code that you _did_ write.

When you get to your code, you want to keep everything the same;  review your code changes like the other changes you've reviewed. For example, I will create a pull request and use the code review functionality in GitHub just like I would for any other pull request. When reviewing code,  I will have my browser window take up the majority of my screen real estate, but also have a terminal open for testing things. This is a different setup than I would have when writing code, and that visual difference is a cue to my brain that we are (still) in review mode. 

Moving away from the owner's mindset is difficult and takes practice. Even when the owner's mindset sticks around, it is still valuable to review your own code. You may not find drastically different approaches to take, but you can often find the low hanging fruit fixes (spelling, style, etc.). You can also use the review as an opportunity to annotate your pull request - help guide your team by explaining key aspects of your changes or calling our areas of concern. 

### Start new work

Depending on your team's mindset around [WIP limits](https://www.planview.com/resources/articles/benefits-wip-limits/), you could also start new work. Part of what can make having multiple units of work in progress harmful to overall team productivity can also help make receiving feedback easier.

When you are starting work on a new problem, you not only physically move on, but also emotionally. You become invested in the new code you are writing, and lose some of the attachment you add to your previous code. This emotional detachment to your old work may make it easier to receive critical feedback on it. 

However, from a project management point of view, this detachment can cause productivity issues. You can end up feeling like the feedback on your old code is just a distraction from the current work you are doing. If the detachment goes too far, it can lead to slow turnaround time and delay getting code into the hands of users.

With these pros and cons in mind, consider using this method more sparingly than other options.

### Step away (again)

Above, we discussed stepping away before receiving feedback. The goal was to create some distance between yourself and the code you are putting up for review to make you more receptive to receiving feedback. We may also want to step away _after_ you've received some feedback. Sometimes, even if we've done our best to prepare ourselves, feedback can still be difficult to receive.

If you find you are getting emotional when reading through feedback, step away.

In this case, I think it makes even more sense to physically step away instead of moving on to a different task. Physical activity can help settle your emotions while still letting you focus on and process the feedback. 

When preparing to request feedback, we were trying to separate ourselves from the code. We almost wanted to forget that we wrote the code so we would receive any feedback more objectively. 

However, after we've gotten feedback, we do not want to discard it right away. We don't want to distract ourselves with other work, but instead, take the time to understand it. This time to process is especially important when it causes an emotional reaction.

I am not saying that you should dwell on poorly given feedback or personal attacks. There is no place for that in code reviews and should not be tolerated. I am saying that if someone is offering you constructive feedback, whether you agree or disagree, you shouldn't turn a blind eye.

I have found that sometimes I am resistant to feedback out of laziness. I have a solution that I spent time on and don't want to re-do everything to take this alternative approach. Depending on the situation, it may or may not make sense to invest in large changes. However, the situation that determines whether it makes sense is not how I'm feeling that day. Instead, it should be based on more objective parameters - do we have a deadline, is either solution objectively better for our needs (speed, memory usage, etc.), does going forward with the current solution make it harder to use the suggested solution, etc. My initial emotional reaction may be, "whelp, that's going to be a pain...let's hold off on this." That's when stepping away can be beneficial. 

When I step away, I find I am less bogged down by the amount of work something will take and feel free to imagine what would be the best option. Stepping away from the keyboard frees me to not worry about work I will be doing and instead let my imagination explore the suggestion. It's much easier for me to imagine the solutions than code it up, so my laziness doesn't get in the way. After this exploration, I may find advantages or disadvantages to either solution. I can now have a more objective and constructive conversation about these. Rather than being driven by emotion, I can continue the conversation productively and objectively.

If I am resistant to feedback, I find stepping away can help ease my initial emotions and help me evaluate the situation more thoughtfully. At the very least, it can help prevent me from making myself look like a jerk by giving an emotional response.

## Conclusion

Code reviews are an important and difficult part of the software development lifecycle. 

Working to separate yourself from the code you write can help to reduce some of the emotional aspects of this process and make it easier for you as the receiver to embrace the value of code reviews. This separation becomes easier with time, space, and an effort to shift mindsets.

While we want everyone to treat each other with respect, when you assume best intentions and have some level of emotional detachment, it can be easier for reviewers to focus more on giving feedback that results in the best final product and less on how they are delivering the feedback.

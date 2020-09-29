---
title: 'Receiving Code Review Feedback'
date: '2020-09-11T06:14:13.265Z'
categories: ['code review', 'soft skills']
---

Code reviews are a common topic among blog posts and videos. This is for a good reason - code reviews are a powerful tool for spreading knowledge, improving code quality, and, when done right, can strengthen team bonds.

Often, content around code reviews is focused on how to _give_ a good code review. In this post, I want to cover how to be on the _receiving_ end.

If you are interested in content on giving a good code review, I recommend the talk [_Implementing a Strong Code-Review Culture_](https://youtu.be/PJjmw9TRB7s) by Derek Prior. It is a great overall introduction to code reviews with a strong focus on how to give a code review that can be well received. It has been hugely impactful on how I give code reviews.

## You are not your code

A blog post about being on the receiving end of a code review may seem superfluous; all you have to do is put the code up and wait for the review, right? Not when our sneaky friend, ego gets involved.

As "knowledge workers," our worth can feel closely tied to the knowledge we bring. For developers, this knowledge often comes in the form of code we deliver. When tied too closely, a lack of knowledge can feel like a lack of worth. When you attach your worth into your code it's going to be hard to receive a code review. Every piece of feedback and discussion is an attack on the ego.

Even on a team with a strong code review culture that is considerate of each other's feelings, code reviews are still providing someone with (often critical) feedback. If you have your ego tied into the code you wrote, even a well-intentioned code review can feel like a personal attack. If they don't like the way you wrote this line of code...do they not like you? Do they think you don't deserve to be on the team? While this jump from a comment on a line of code to worrying about job security can feel over-the-top, I know from experience it happens.

 Instead, remember, **you are not your code**. 

While much easier said than done, I have a few practices that can help make it easier to be less emotionally attached to the code you have up for review and more receptive to receiving feedback.

### Review your code first

This may sound similar to the concept of self-deprecating humor - make fun of yourself before anyone else can. However, the intention is not to beat others to the punch, but instead to **change your relationship with your code**.  You want to shift from the owner's mindset to the reviewer's mindset to change how you are thinking about the code you wrote.

The owner's mindset is protective of the code and attached to solutions. It went through the difficult journey of getting to the solution and may have a hard time seeing other options. On the other hand, the reviewer's mindset lacks the baggage of the journey. It comes in open for anything. This openness can lead to seeing improvement opportunities that you become blind to as the owner.

You should review your code just like you would anyone else's. By getting into the same "mode" you would when reviewing a teammate's code, you set your brain up to realize, "oh hey, it's code review time" and shift mindsets.  

To help get into this mode use the same "environment" as you would any other code review. For example, I will create a pull request and use the code review functionality in GitHub just like I would for any other pull request. This has an added benefit that I can also annotate my pull request in areas I think others may have questions, or call out sections of code I think could be better but am unsure of how to improve.

This mental shift can be difficult. You may find it easier to shift mindsets if you review other pull requests before you review your own. Your teammates will appreciate the feedback and you will be more apt to treat your pull request just like the others you have been reviewing.

### Step away

Stepping away is advice that can be given in all sorts of situations, including preparing for code review feedback. When you receive feedback on something you just worked on it is more difficult to receive it objectively.

Stepping away can be as simple as checking chat notifications after you've pushed your code. However, it can be powerful to have physical separation. If you can, take a break from screen time. This can be something quick like refilling your drink or longer like taking a walk. You want to create space between yourself and the code you are soliciting feedback on. As silly as it may sound, I have found that actual, physical space can help me feel less attached to my code.

Time is precious and we don't want to get in the way of shipping code, but I think this time is worth it. Depending on how you do reviews, it likely won't even have to block reviews from getting done. If your team primarily does asynchronous reviews, you can push your code, request reviews, and step away. If your team does synchronous reviews, you can find a future time that works for the reviewer before taking your break.

#### Stepping away productively

In addition to taking breaks to step away from your code, there are productive options to further separate your "self" from your pull requests.

You can review code for other people on your team. As mentioned above this is also a great way to set yourself up to review your code more objectively.

Depending on your team's mindset around [WIP limits](https://www.planview.com/resources/articles/benefits-wip-limits/) you could also start new work. Part of what can make having multiple units of work in progress harmful to overall team productivity can also help make receiving feedback easier.

When you are starting work on a new problem, you often move on (emotionally) from the old one and become invested in the new problem. This emotional detachment to your old work may make it easier to receive critical feedback on it.

From a project management point of view, this detachment can cause productivity issues though. You can end up feeling like the feedback on your boring, terrible, old code is just a distraction from your interesting, perfect, new code.

With these pros and cons in mind, consider using this method more sparingly than other options.

### Step away (again)

Above, we discussed stepping away before receiving feedback. The goal was to create some distance between yourself and the code you are putting up for review in order to make you more receptive to receiving feedback. We may also want to step away _after_ you've received some feedback. Sometimes, even if we've done our best to prepare ourselves, feedback can still be difficult to receive.

If you find you are getting emotional when reading through feedback, step away.

In this case I think it makes more sense to physically step away instead of moving on to a different task. Physical activity can help settle your emotions while still letting you focus on and process the feedback. When preparing to request feedback, we wanted to almost forget about the code we wrote to not be defined by our code. When receiving feedback, we do not want to immediately discard it. We need to understand it. This is especially true when it causes an emotional reaction.

I am not saying that you should dwell on improperly given feedback or personal attacks. There is no place for that in code reviews and should not be tolerated. I am saying that if someone is offering you constructive feedback, whether you agree or disagree, you shouldn't turn a blind eye.

I have found that sometimes I am resistent to feedback out of laziness. I have a solution that I spent time on, and don't want to re-do everything to take this alternative approach. Depending on the situation, it may or may not make sense to invest in large changes. However, the situation that determines whether it makes sense is not how I'm feeling that day. Instead, it should be based on more objective parameters - do we have a deadline, is either solution objectively better for our needs (speed, memory usage, etc.), does going forward with the current solution make it harder to use the suggested solution, etc. My initial emotional reaction may be, "whelp, that's going to be a pain...let's hold off on this." That's when stepping away can be beneficial. 

When I step away I find I am less bogged down by the amount of work something will take and am more free to imagine what is best. Stepping away from the keyboard frees me to not worry about work I will be doing and instead let my imagination explore the suggestion. It's much easier for me to imagine the solutions than code it up, so it doesn't feel like as hard of work. After this exploration, I may find advantages or disadvantages to either solution. I can now have a more objective and constructive conversation about these. Rather than being driven by emotion, I can continue the conversation productively and objectively.

If I am resistent to feedback, I find stepping away can help ease my initial emotions and help me evaluate the situation in a more thoughtful manner. At the very least, it can help prevent me from making myself look like a jerk by giving an emotional response.

## Conclusion

Code reviews are an important and difficult part of the software development lifecycle. Working to separate yourself from the code you write can help to reduce some of the emotional aspects of this process and make it easier on you as the receiver as well as though providing you with the code review. While we want everyone to treat each other with respect, when you assume best intent and have some level of emotional detachment it can be easier for reviewers to focus more on giving feedback that results in the best final product and less on how they are delivering the feedback.

* Time allowing, can help reset mindset of wanting to get a quick approval
* why you do it
  1. Reframe the goal of code reviews - not to get approval, but to improve code and learn
  1. "your" code versus the team's code
* Closing (tie to how to give code review talks)
1. think of it like writing - it's  a draft
* Exposure
  * do it more
  * pair - get real time feedback
*


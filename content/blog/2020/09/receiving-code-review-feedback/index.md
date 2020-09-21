---
title: 'Receiving Code Review Feedback'
date: '2020-09-11T06:14:13.265Z'
categories: ['code review', 'soft skills']
---

Code reviews are a common topic among blog posts and videos. This is for a good reason - code reviews are powerful tool for spreading knowledge, improving code quality, and, when done right, can strengthen team bonds.

Often, content around code reviews is focused on how to _give_ a good code reivew. In the post, I want to cover how to manage being on the _receiving_ end.

If you are interested in content on giving a good code review, I recommend the talk [_Implementing a Strong Code-Review Culture_](https://youtu.be/PJjmw9TRB7s) by Derek Prior. It is a great overall introduction to code reviews with a strong focus on how to give a code review that can be well received. It has been hugely impactful on how I give code reviews.

## You are not your code

A blog post about being on the receiving end a code review may seem surperfulous; all you have to do is put the code up and wait for the review, right? Not when our sneaky friend, ego gets involved.

In a worst case scenario (and the reason there is so much content on how to give better code reviews), they can feel like (or actually be) personal attacks. In a best case scenario, code reviews are a form of receiving feedback on your work.

If you have your ego tied into the code you wrote, even a well-intentioned code review can feel like a personal attack. If they don't like the way your wrote this line of code...do they not like you? Do they think you don't deserve to be on the team? While this jump from a comment on a line of code to worrying about job security can feel over-the-top, I have experienced these emotional overreactions.

I believe as "knowledge workers," our worth can feel closely tied to the knowledge we bring. When tied too closely, a lack of knowledge can feel like a lack of worth.

<!-- On top of tying our worth into the correctness of our code, writing code also has an artistic element to it. Different developers have different styles and aesthetic preferences for their code. Even when trying to be rational and Spock-like about the functionality of code, sometimes feedback can be about more "artistic" choices you have made and. This feedback can be even more impactful because it is a tied to personal preference over fact. -->

When you attach your worth into your code it's going to be hard to receive a code review. Every piece of feedback and discussion is an attack on the ego. Instead, remember, **you are not your code**.

<!--It's okay to work hard on something, and for others to find opportunities to improve it. -->
While much easier said than done, I have a few practices that can help make it easier to be less emotionally attached to the code you have up for review and more receptive to receiving feedback.

1. Review your own code first

This can feel similar to the concept of self-deprecating humor - make fun of yourself before anyone else can. However, the intention is not to beat others to the punch, but instead to **change your relationship with your code**. You want to shift your mindset from writer to reviewer to change how you are thinking about the code you wrote.

You should review your own code just like you would anyone else's. By getting into the same "mode" you would to review anyone else's code, you set your brain up to realize, "oh hey, it's code review time."

To help get into this mode use the same "environment" as you would any other code review. For example, I will create my own pull request and use the code review functionality just like I would for any other pull request. This has an added benefit that I can also annotate my pull request in area I think other may have questions or call out sections of code I think could be better am not sure how to improve.

This mental shift can be difficult. You may find it easier to shift mindsets if review other pull requests before you review your own. Your teammates will appreciate the feedback and you will be more apt to treat your pull request just like the others you have been reviewing. 

1. Step away

Stepping away can be advice given to all sort of situations, including preparing for code review feedback. When you receive feedback on something "hot of the presses" it's much more difficult to receive it objectively.

Stepping away can be as simple as checking email after you've pushed your code. However, it can be powerful to have physical separation. If you can, take a break from screen time. This can be something quick like refilling your drink or longer like taking a walk. You want to create space between yourself and the code you are soliciting feedback on. As silly as it may sound, I have found this actual, physical space can help me feel less attached to my code.

Time is precious and we don't want to get in the way of shipping code, but I think this time can be worth it. In all honesty, depending on how you do reviews, it probably doesn't even have to block reviews from getting done. If your team primarily does asynchronous review, you can push your code, request reviews, and step away. If you team does synchronous reviews, you can find a future time that works for the reviewer before taking your break. 

In addition to take breaks to step away from your code, there are productive options to further separate your "self" from your pull requests.

You can review code for other people on your team. As mentioned above this is also a great way to set yourself up to review your own code more objectively.

Depending on your team's mindset around [WIP limits](https://www.planview.com/resources/articles/benefits-wip-limits/) you could also start new work. Personally, I try to limit this and prefer reviewing other code to help move work through the system, but sometimes it's the best option. Part of what can make having multiple units of work in progress harmful to overall team productivity can also help make receiving feedback easier. When you are starting work on a new problem, you often move on from the old one. When you being receiving feedback on your old work, you will likely feel less emotionally attached because your emotions are tied up in your new work. You may find that you can more easily critical feedback ("You're right, I can't believe I used to write such bad code (yesterday)"). From a project management point of view this detachment can cause productivity issues though. You can end up feeling like the feedback on your boring, terrible, old code is just a distraction from your perfect, new code. With these pros and cons in mind, consider using this method more sparingly than other options.

* also step away after reading review
* step away productively
  * review other code
  * maybe start new pr (this can really give separation, but lean agile issues)

* separate code from self
  1. Review your own code first
    * This puts you in the mindset of looking for issues with the code
  1. Step away
    * Try to develop some mental separation
    * Possibly starting on other work
    * Time allowing, can help reset mindset of wanting to get a quick approval
* why you do it
  1. Reframe the goal of code reviews - not to get approval, but to improve code and learn
  1. "your" code versus the team's code
* Closing (tie to how to give code review talks) 
1. Assume best intention
  * It's a team effort
  * Maybe someone was in a rush and wanted to get comments out
* unknown
1. think of it like writing - it's  a draft
* Exposure
  * do it more
  * pair - get real time feedback
* 

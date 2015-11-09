---
layout: post
title:  "Picks for the week of November 8, 2015"
date:   2015-11-08 22:53:02
categories: picks
---

This week I decided to take a long weekend and spend it in New York City. This week's picks are inspired by this short trip.

## NYC Coffee Shop Hipster

This will come as no surprise to most of you - the morning of my flight was fraught with various connections issues starting with SSL failures and escalating to not even being able to connect to our application servers. These seemed to mostly taper off after some restarts and


## Vim Command of the Week

Months into Vim and I am still leaning even basic navigation commands. The most useful command I learned about this week was `ge`. From Vim's Help:

```
ge      Backward to the end of word [count] |inclusive|.
gE      Backward to the end of WORD [count] |inclusive|.
```

These commands allows you to move to the of the previous word or WORD respectively.

It took a while for me to discover these commands because there are alternatives command combinations that you learn likely learn early and, most of the time, do what you want.

Prior to learning about these commands I would generally just go back a word (`b`) or WORD (`B`) then go to the end of the word (`e`) or WORD (`E`). Occasionally this can be done with only two keystrokes (`be` or `BE`) if you are between two words or want to go go to then end of you current work (but you should just be using `e` in that case). Most of the time your cursor will be on the next word, requiring you go to go `b`ack two or more words.

This slight difference between combining `b` and `e` and using `ge` may seem minimal but that is what make Vim nerds love Vim!
